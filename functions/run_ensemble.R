run_hybrid <- function() {
  #
}


get_forecast_combination <- function(y_val, y_test, lasso, postlasso, bagging, rf) {
  
  # 1. Construct validation and test forecast matrices
  
  F_val <- cbind(
    LASSO = as.numeric(lasso$pred_val),
    Post_LASSO = as.numeric(postlasso$pred_val),
    Bagging = as.numeric(bagging$pred_val),
    Random_Forest = as.numeric(rf$pred_val)
  )
  
  F_test <- cbind(
    LASSO = as.numeric(lasso$pred_test),
    Post_LASSO = as.numeric(postlasso$pred_test),
    Bagging = as.numeric(bagging$pred_test),
    Random_Forest = as.numeric(rf$pred_test)
  )
  
  # Check dimensions
  stopifnot(
    nrow(F_val) == length(y_val),
    nrow(F_test) == length(y_test),
    ncol(F_val) == ncol(F_test)
  )
  
  # Remove incomplete validation observations
  valid_val <- complete.cases(y_val, F_val)
  
  F_val <- F_val[valid_val, , drop = FALSE]
  y_val <- y_val[valid_val]
  
  # Remove incomplete test observations
  valid_test <- complete.cases(y_test, F_test)
  
  F_test <- F_test[valid_test, , drop = FALSE]
  y_test <- y_test[valid_test]
  
  k <- ncol(F_val)
  
  # 2. Constrained GR weights: no intercept
  # Sum of weights = 1
  # All weights >= 0
  
  gr_constrained <- lsei(
    A = F_val,
    B = y_val,
    E = matrix(1, nrow = 1, ncol = k),
    F = 1,
    G = diag(k),
    H = rep(0, k)
  )
  
  weights_constrained <- as.numeric(gr_constrained$X)
  
  forecast_constrained <- as.numeric(F_test %*% weights_constrained)
  
  # 3. Constrained GR weights: with intercept
  # Intercept is unrestricted
  # Model weights sum to 1 and are non-negative
  
  F_val_intercept <- cbind(Intercept = 1, F_val)
  F_test_intercept <- cbind(Intercept = 1, F_test)
  
  E_intercept <- matrix(c(0, rep(1, k)),
    nrow = 1
  )
  
  G_intercept <- cbind(rep(0, k), diag(k))
  
  gr_intercept <- lsei(
    A = F_val_intercept,
    B = y_val,
    E = E_intercept,
    F = 1,
    G = G_intercept,
    H = rep(0, k)
  )
  
  weights_intercept <- as.numeric(gr_intercept$X)
  
  forecast_intercept <- as.numeric(F_test_intercept %*% weights_intercept)
  
  # 4. Unrestricted GR weights: no intercept
  
  gr_unrestricted <- lm.fit(x = F_val, y = y_val)
  
  weights_unrestricted <- gr_unrestricted$coefficients
  
  forecast_unrestricted <- as.numeric(F_test %*% weights_unrestricted)
  
  # 5. Unrestricted GR weights: with intercept
  
  gr_unrestricted_intercept <- lm.fit(x = F_val_intercept, y = y_val)
  
  weights_unrestricted_intercept <- gr_unrestricted_intercept$coefficients
  
  forecast_unrestricted_intercept <- as.numeric(
    F_test_intercept %*% weights_unrestricted_intercept
  )
  
  # 6. Calculate test MSE
  
  MSE <- function(actual, predicted) {
    mean((actual - predicted)^2)
  }
  
  test_forecasts <- data.frame(
    actual = y_test,
    GR_constrained = forecast_constrained,
    GR_constrained_intercept = forecast_intercept,
    GR_unrestricted = forecast_unrestricted,
    GR_unrestricted_intercept = forecast_unrestricted_intercept
  )
  
  test_metrics <- data.frame(
    method = c(
      "GR constrained",
      "GR constrained with intercept",
      "GR unrestricted",
      "GR unrestricted with intercept"
    ),
    MSE = c(
      MSE(y_test, forecast_constrained),
      MSE(y_test, forecast_intercept),
      MSE(y_test, forecast_unrestricted),
      MSE(y_test, forecast_unrestricted_intercept)
    )
  )
  
  # 7. Return weights, forecasts and metrics
  
  list(
    weights = list(
      constrained = setNames(
        weights_constrained,
        colnames(F_val)),
      constrained_intercept = setNames(
        weights_intercept,
        c("Intercept", colnames(F_val))),
      unrestricted = setNames(
        weights_unrestricted,
        colnames(F_val)),
      unrestricted_intercept = setNames(
        weights_unrestricted_intercept,
        c("Intercept", colnames(F_val)))
    ),
    
    forecasts = test_forecasts,
    metrics = test_metrics
  )
}

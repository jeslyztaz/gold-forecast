# Forecast combinations of the individual models (Granger-Ramanathan weights fitted on the VALIDATION block,
# applied to the TEST block). `models` is a named list; each element has pred_val and pred_test, as returned by
# run_rolling_rlasso(), run_bagging(), run_rf() and get_historical_average().
#
# lsei() is the `lsei` package: minimise ||a w - b||^2 subject to c w = d and w >= lower; it returns the weight vector.
# Returns pred_test (named list of test forecasts, one per combination) and the weights.
run_ensemble <- function(md, models) {
  y_val <- md$y[md$val_idx]
  F_val  <- sapply(models, function(m) as.numeric(m$pred_val))
  F_test <- sapply(models, function(m) as.numeric(m$pred_test))
  stopifnot(nrow(F_val) == length(y_val), nrow(F_test) == length(md$test_idx), !anyNA(F_test))
  
  ok <- complete.cases(y_val, F_val)                  # weights are estimated on complete validation rows only
  F_val <- F_val[ok, , drop = FALSE]; y_val <- y_val[ok]
  k <- ncol(F_val); nm <- colnames(F_val)
  
  F_val_i  <- cbind(Intercept = 1, F_val)
  F_test_i <- cbind(Intercept = 1, F_test)
  
  # 1. constrained (weights >= 0, sum to 1), no intercept
  w1 <- as.numeric(lsei(a = F_val, b = y_val, c = rep(1, k), d = 1, lower = 0))
  # 2. constrained, with an unrestricted intercept
  w2 <- as.numeric(lsei(a = F_val_i, b = y_val, c = c(0, rep(1, k)), d = 1, lower = c(-Inf, rep(0, k))))
  # 3. unrestricted OLS weights, no intercept   4. unrestricted OLS weights, with intercept
  w3 <- lm.fit(x = F_val,   y = y_val)$coefficients
  w4 <- lm.fit(x = F_val_i, y = y_val)$coefficients
  w3[is.na(w3)] <- 0; w4[is.na(w4)] <- 0
  
  list(
    pred_test = list(
      "Ens: GR constrained"                = as.numeric(F_test   %*% w1),
      "Ens: GR constrained + intercept"    = as.numeric(F_test_i %*% w2),
      "Ens: GR unrestricted"               = as.numeric(F_test   %*% w3),
      "Ens: GR unrestricted + intercept"   = as.numeric(F_test_i %*% w4)),
    weights = list(constrained = setNames(w1, nm), constrained_intercept = setNames(w2, c("Intercept", nm)),
                   unrestricted = setNames(w3, nm), unrestricted_intercept = setNames(w4, c("Intercept", nm)))
  )
}
library(sandwich)

# craft dataframe for metrics
make_forecast_results <- function(df, indice=2, predictions, model, horizon=1) {
  test <- df[df$date >= test_start,]
  dates <- test$date
  actual <- test[,indice]
  tibble(
    date = dates,
    horizon = horizon,
    model = model,
    actual = actual,
    forecast = as.numeric(predictions)
  )
}

# statistical accuracy
calculate_r2oos <- function(p, pi, ha) {
  valid <- complete.cases(p, pi, ha)
  
  actual    <- actual[valid]
  forecast  <- forecast[valid]
  benchmark <- benchmark[valid]
  
  return(100 * (1 - sum((actual - forecast)^2) / sum((actual - benchmark)^2)))
}

calculate_cw <- function(p, pi, ha) {
  valid <- complete.cases(p, pi, ha)
  
  actual    <- actual[valid]
  forecast  <- forecast[valid]
  benchmark <- benchmark[valid]
  
  e_b <- actual - benchmark
  e_m <- actual - forecast
  
  # Clark-West adjusted loss differential
  f <- e_b^2 - e_m^2 + (benchmark - forecast)^2
  
  # HAC standard error, allowing for overlapping forecast horizons
  # fit <- lm(f ~ 1)
  
  V <- NeweyWest(fit, lag = 1, prewhite = FALSE, adjust = TRUE)
  cw_stat <- coef(fit)[1] / sqrt(V[1, 1])
  p_value <- 1 - pnorm(cw_stat)
  
  return(c(CW_statistic = cw_stat, p_value = p_value))
}

# economic value
calculate_economic_metrics <- function(
    actual_excess,
    forecast_excess,
    variance_forecast,
    rf,
    gamma = 3,
    periods_per_year = 252
) {

  valid <- complete.cases(
    actual_excess,
    forecast_excess,
    variance_forecast,
    rf
  )

  actual_excess   <- actual_excess[valid]
  forecast_excess <- forecast_excess[valid]
  variance_forecast <- variance_forecast[valid]
  rf <- rf[valid]

  # Portfolio weights
  weight <- (1 / gamma) *
    forecast_excess / variance_forecast

  # Portfolio returns
  portfolio_return <- (1 - weight) * rf + weight * actual_excess

  # Mean and variance
  mu <- mean(portfolio_return)
  sigma2 <- var(portfolio_return)

  # Certainty equivalent return
  CER <- mu - 0.5 * gamma * sigma2

  # Terminal wealth
  TW <- prod(1 + portfolio_return)

  # Annualised Sharpe ratio
  SR <- sqrt(periods_per_year) * mean(portfolio_return) / sd(portfolio_return)

  c(CER = CER, TW = TW, SR = SR)
}
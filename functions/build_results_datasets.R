# One long table of TEST-period forecasts: one row per (date, model).
#   forecast       forecast of the target (log return or dollar change, as chosen in prepare_model_data)
#   ret_forecast   the same forecast as a simple return (used for the trading rules)
#   price_forecast / price_actual   gold price at t+h, backed out from the forecast / realised
#   gret, rf, sig2 realised gold return, risk-free rate and variance forecast, used by the trading rules
make_forecast_results <- function(md, pred_test, model) {
  stopifnot(length(pred_test) == length(md$test_idx))
  i <- md$test_idx; f <- as.numeric(pred_test); P0 <- md$P[i]
  ret_f <- if (md$target == "logreturn") exp(f) - 1 else f / P0
  tibble(date = md$date[i], horizon = md$h, model = model, actual = md$y[i], forecast = f,
         ret_forecast = ret_f, price_forecast = P0 * (1 + ret_f), price_actual = P0 * (1 + md$gret[i]),
         gret = md$gret[i], rf = md$rf[i], sig2 = md$sig2[i])
}

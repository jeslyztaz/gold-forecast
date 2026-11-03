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
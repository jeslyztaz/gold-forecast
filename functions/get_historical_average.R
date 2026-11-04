# Historical average benchmark: at origin t, the mean of all h-day returns that were already REALISED at t,
# i.e. y_s for s <= t - h (the expanding window used by every other model).
get_historical_average <- function(md) {
  n <- length(md$y); h <- md$h
  pred <- rep(NA_real_, n)
  idx <- (h + 1):n
  pred[idx] <- cumsum(md$y)[idx - h] / (idx - h)
  list(pred_val = pred[md$val_idx], pred_test = pred[md$test_idx])
}

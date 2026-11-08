get_historical_average <- function(md) {
  n <- length(md$y)
  h <- md$h
  pred <- rep(NA_real_, n)
  idx <- (h + 1):n
  pred[idx] <- cumsum(md$y)[idx - h] / (idx - h)
  return(pred_test = pred[md$test_idx])
}
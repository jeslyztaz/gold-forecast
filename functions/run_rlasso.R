run_rlasso <- function(md, post = FALSE) {
  p <- ncol(md$X)
  ev <- md$test_idx
  f <- length(md$test_idx)
  sel <- matrix(0, f, p, dimnames = list(NULL, colnames(md$X)))
  
  for (i in seq_len(f)) {
    t  = ev[i]
    tr <- 1:(t - md$h)
    m <- rlasso(md$X[tr, ], md$y[tr], post = post, intercept = TRUE)
    b <- m$coefficients # (Intercept), then one per predictor
    b[is.na(b)] <- 0
    f[i] <- b[1] + sum(md$X[t, ] * b[-1]) # forecast = intercept + x_t' beta
    sel[i, ] <- as.numeric(b[-1] != 0)
  }
  
  return(list(pred_test  = f, n_selected = rowSums(sel), selected = sel == 1))
}

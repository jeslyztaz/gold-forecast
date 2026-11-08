run_ensemble <- function(md, models, ha, theta = 0.9, min_obs = 60) {
  Fev <- sapply(models, function(m) m$pred_test) # test forecasts, one column per model
  yev <- md$y[md$test_idx]
  stopifnot(nrow(Fev) == length(yev), !anyNA(Fev))
  k <- ncol(Fev); nm <- colnames(Fev); n_val <- length(md$val_idx); n_test <- length(md$test_idx)
  
  w_dmspe <- w_con <- matrix(1 / k, n_test, k, dimnames = list(NULL, nm))
  w_unc   <- matrix(NA_real_, n_test, k + 1, dimnames = list(NULL, c("Intercept", nm)))
  f_dmspe <- f_con <- f_unc <- numeric(n_test)
  
  for (j in seq_len(n_test)) {
    i <- n_val + j # position of this test origin in Fev
    past <- seq_len(i - md$h) # forecasts whose target is realised by t
    Fi <- Fev[i, ]
    if (length(past) >= min_obs) {
      A <- Fev[past, , drop = FALSE]; b <- yev[past]
      # DMSPE: weights inversely proportional to discounted past squared errors (theta = 0.9)
      phi <- colSums(theta^(max(past) - past) * (b - A)^2)
      w_dmspe[j, ] <- (1 / phi) / sum(1 / phi)
      # Granger-Ramanathan: (i) constrained, weights >= 0 summing to 1, no intercept; 
      # (ii) unrestricted OLS with intercept
      w_con[j, ] <- as.numeric(lsei(a = A, b = b, c = rep(1, k), d = 1, lower = 0))
      cf <- lm.fit(cbind(1, A), b)$coefficients; cf[is.na(cf)] <- 0
      w_unc[j, ] <- cf
    } else {
      w_unc[j, ] <- c(0, rep(1 / k, k)) # too little history: equal weights
    }
    f_dmspe[j] <- sum(Fi * w_dmspe[j, ])
    f_con[j] <- sum(Fi * w_con[j, ])
    f_unc[j] <- w_unc[j, 1] + sum(Fi * w_unc[j, -1])
  }
  
  F_test <- Fev[n_val + seq_len(n_test), , drop = FALSE]
  f_mean <- rowMeans(F_test)
  return(list(
    pred_test = list(
      "Ens: mean" = f_mean,
      "Ens: median" = apply(F_test, 1, median),
      "Ens: mean + HA" = 0.5 * f_mean + 0.5 * as.numeric(ha$pred_test),
      "Ens: DMSPE" = f_dmspe,
      "Ens: GR constrained" = f_con,
      "Ens: GR unconstrained" = f_unc),
    weights = list(
      date = md$date[md$test_idx], 
      dmspe = w_dmspe, 
      gr_constrained = w_con, 
      gr_unconstrained = w_unc)
  ))
}
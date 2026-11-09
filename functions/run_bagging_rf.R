run_bagging_rf <- function(md, ntree = 500, refit_every = 21, nodesize = 5, min_train = 250, seed = 4308) {
  set.seed(seed)
  ev   <- md$test_idx
  n_ev <- length(ev)
  p    <- ncol(md$X)
  vnames <- colnames(md$X)
  
  # a. tune mtry for RF once, on the initial training window (OOB error)
  tr0       <- 1:(ev[1] - md$h)
  mtry_grid <- unique(pmax(1, round(c(p / 6, p / 4, p / 3, p / 2))))
  
  oob_tune  <- sapply(mtry_grid, function(mt) {
    rf <- randomForest(md$X[tr0, ], md$y[tr0], ntree = ntree, mtry = mt, nodesize = nodesize)
    tail(rf$mse, 1) # OOB mean squared error
  })
  
  mtry_rf <- mtry_grid[which.min(oob_tune)]
  print(data.frame(mtry = mtry_grid, OOB_MSE = oob_tune))
  cat("Chosen mtry for RF:", mtry_rf, " (bagging uses mtry =", p, ")\n")
  
  # b. expanding-window forecasts
  f_bag <- f_rf <- numeric(n_ev)
  imp_bag <- imp_rf <- setNames(numeric(p), vnames)
  oob_bag <- oob_rf <- numeric(ntree)
  n_refit <- 0
  
  for (i in seq_len(n_ev)) {
    t <- ev[i]
    if ((i - 1) %% refit_every == 0) { # refit models
      tr <- 1:(t - md$h) # only targets already realised at t
      stopifnot(length(tr) >= min_trin)
      m_bag <- randomForest(md$X[tr, ], md$y[tr], ntree = ntree, mtry = p, nodesize = nodesize)
      m_rf  <- randomForest(md$X[tr, ], md$y[tr], ntree = ntree, mtry = mtry_rf, nodesize = nodesize)
      m_rfd <- randomForest(md$X[tr, ], md$y[tr], ntree = ntree) # all settings at their defaults
      imp_bag <- imp_bag + importance(m_bag)[, "IncNodePurity"]; oob_bag <- oob_bag + m_bag$mse
      imp_rf  <- imp_rf  + importance(m_rf)[,  "IncNodePurity"]; oob_rf  <- oob_rf  + m_rf$mse
      imp_rfd <- imp_rfd + importance(m_rfd)[, "IncNodePurity"]; oob_rfd <- oob_rfd + m_rfd$mse
      n_refit <- n_refit + 1
      cat("Forests refit", n_refit, "| origin", i, "of", n_ev, "\n")
    }
    f_bag[i] <- predict(m_bag, md$X[t, , drop = FALSE])
    f_rf[i]  <- predict(m_rf,  md$X[t, , drop = FALSE])
    f_rfd[i] <- predict(m_rfd, md$X[t, , drop = FALSE])
  }
  
  te <- seq_along(md$test_idx)
  return(list(
    bagging = list(pred_test = f_bag[te], importance = imp_bag / n_refit,
                   oob_mse = oob_bag / n_refit),
    rf = list(pred_test = f_rf[te], importance = imp_rf / n_refit, oob_mse = oob_rf / n_refit, 
              mtry = mtry_rf, mtry_tuning = data.frame(mtry = mtry_grid, oob_mse = oob_tune)),
    rf_default = list(pred_test = f_rfd[te], importance = imp_rfd / n_refit, 
                      oob_mse = oob_rfd / n_refit, mtry = floor(p / 3))))
}
# Expanding-window bagging and random forest (randomForest package, default settings except mtry).
#   bagging       : mtry = p (every split considers all predictors)
#   random forest : package default for regression, mtry = floor(p / 3)
# Same timing rules as run_rolling_rlasso(): training rows s <= t - h, refit every `refit_every` origins.
# Besides the forecasts it returns, for plots.R: `importance` (IncNodePurity averaged over the refits) and
# `oob_mse` (out-of-bag MSE after 1..ntree trees, averaged over the refits).
run_forest <- function(md, mtry, ntree = 500, refit_every = 63, min_train = 250, seed = 759) {
  set.seed(seed)
  ev   <- c(md$val_idx, md$test_idx)
  pred <- rep(NA_real_, length(md$y))
  imp  <- setNames(numeric(ncol(md$X)), colnames(md$X)); oob <- list()

  for (st in seq(1, length(ev), by = refit_every)) {
    blk <- ev[st:min(st + refit_every - 1, length(ev))]
    tr  <- seq_len(blk[1] - md$h)
    stopifnot(length(tr) >= min_train)
    fit <- randomForest(x = md$X[tr, , drop = FALSE], y = md$y[tr], ntree = ntree, mtry = mtry)
    pred[blk] <- predict(fit, newdata = md$X[blk, , drop = FALSE])
    imp <- imp + importance(fit)[, 1]; oob[[length(oob) + 1]] <- fit$mse
    cat("forest (mtry =", mtry, ") refit", st, "of", length(ev), "\n")
  }
  list(pred_val = pred[md$val_idx], pred_test = pred[md$test_idx],
       importance = imp / length(oob), oob_mse = Reduce(`+`, oob) / length(oob))
}

run_bagging <- function(md, ...) run_forest(md, mtry = ncol(md$X), ...)
run_rf      <- function(md, ...) run_forest(md, mtry = max(1, floor(ncol(md$X) / 3)), ...)

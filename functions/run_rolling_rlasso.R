# Expanding-window LASSO / post-LASSO with hdm::rlasso (plug-in penalty, no tuning parameter).
# post = FALSE -> LASSO, post = TRUE -> post-LASSO.
# The model is re-estimated every `refit_every` origins; between refits the coefficients are kept fixed.
# At an origin t the training rows are s <= t - h only (targets already realised), so there is no look-ahead.
# Returns validation and test forecasts in the layout the ensemble and metrics expect, plus `selected`:
# a (refits x predictors) TRUE/FALSE matrix of which predictors rlasso kept at each refit (used by plots.R).
run_rolling_rlasso <- function(md, post = FALSE, refit_every = 21, min_train = 250) {
  ev   <- c(md$val_idx, md$test_idx)                  # contiguous origins
  pred <- rep(NA_real_, length(md$y)); nsel <- rep(NA_integer_, length(md$y))
  sel  <- matrix(NA, 0, ncol(md$X), dimnames = list(NULL, colnames(md$X))); refit_dates <- NULL

  for (st in seq(1, length(ev), by = refit_every)) {
    blk <- ev[st:min(st + refit_every - 1, length(ev))]
    tr  <- seq_len(blk[1] - md$h)                     # rows whose target is known at the first origin of the block
    stopifnot(length(tr) >= min_train)
    fit <- rlasso(md$X[tr, , drop = FALSE], md$y[tr], post = post)
    stopifnot(length(fit$beta) == ncol(md$X))
    # forecast = intercept + X * beta (same as predict(), but no stored formula is needed)
    pred[blk] <- drop(fit$intercept + md$X[blk, , drop = FALSE] %*% as.matrix(fit$beta))
    nsel[blk] <- sum(fit$index)
    sel <- rbind(sel, as.logical(fit$index)); refit_dates <- c(refit_dates, format(md$date[blk[1]]))
    cat("rlasso", if (post) "(post)" else "", "refit", st, "of", length(ev), "| selected:", sum(fit$index), "\n")
  }
  rownames(sel) <- refit_dates
  list(pred_val = pred[md$val_idx], pred_test = pred[md$test_idx], n_selected = nsel[ev], selected = sel)
}

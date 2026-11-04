# Turns the raw forecast dataset into the ONE object that every model, the ensemble and the metrics use.
#
# Timing convention (this is what makes the forecasts real forecasts):
#   * a row is a forecast ORIGIN t = the close of day t; everything in X is known at the close of day t
#   * the target on row t is realised at t+h and is one of (argument `target`):
#       "logreturn" : y_t = log(P_{t+h} / P_t)   (h-day-ahead gold log return)
#       "change"    : y_t = P_{t+h} - P_t        (h-day-ahead dollar change in the gold price)
#     Either way the price forecast is backed out as P_t + (forecast change)  or  P_t * exp(forecast log return)
#   * y_t is only known at t+h, so a model fitted at origin t may only use rows s <= t - h
#
# Predictors are stationary transformations (returns / deviations / spreads), NOT price levels, and never
# include `date` or the target. Macro and uncertainty series are lagged `macro_lag` extra days (publication delay).
#
# Returns a list:
#   X, y, date        one row per usable origin (X is a numeric matrix with column names)
#   P, gret           gold price at the origin, and the realised simple gold return over the next h days
#   h, target         horizon and target type
#   rf                h-day risk-free rate known at the origin (3-month T-bill, WTB3MS, lagged one day)
#   sig2              variance forecast of the h-day return (h x rolling variance of daily log returns)
#   val_idx, test_idx row positions (in X / y / date) of the validation and test origins
prepare_model_data <- function(data, h = 1, val_start, test_start, target = c("logreturn", "change"), macro_lag = 1, vol_win = 252) {
  target <- match.arg(target)
  data <- data[order(data$date), ]
  need <- c("date", "close_usd_per_troy_oz", "ma5", "ma20", "silver_close_usd_per_troy_oz", "SP500", "DTWEXBGS",
            "DAAA", "DBAA", "DGS10", "T10YIE", "WTB3MS", "TMS", "DFY", "VIXCLS", "USEPUINDXD", "RVOL_20", "RVOL_60")
  miss <- setdiff(need, names(data))
  if (length(miss)) stop("Column(s) missing from the dataset: ", paste(miss, collapse = ", "))

  lag_k  <- function(x, k) if (k == 0) x else c(rep(NA_real_, k), head(x, -k))
  lead_k <- function(x, k) c(x[-seq_len(k)], rep(NA_real_, k))
  dlog   <- function(x, k = 1) log(x) - log(lag_k(x, k))          # k-day log change, known at t

  p  <- data$close_usd_per_troy_oz
  lp <- log(p)
  r1 <- dlog(p, 1)                                                  # daily log return

  # ---- target: h-day-ahead log return or dollar change; gret is the realised simple return (used by the trading rule)
  y    <- if (target == "logreturn") lead_k(lp, h) - lp else lead_k(p, h) - p
  gret <- lead_k(p, h) / p - 1

  # ---- predictors known at the close of day t
  sil <- data$silver_close_usd_per_troy_oz
  gs  <- log(p / sil)
  F <- data.frame(
    gold_ret1 = r1, gold_ret5 = dlog(p, 5), gold_ret20 = dlog(p, 20),
    gold_dev_ma5 = log(p / data$ma5), gold_dev_ma20 = log(p / data$ma20),
    gold_abs_ret1 = abs(r1), RVOL_20 = data$RVOL_20, RVOL_60 = data$RVOL_60,
    silver_ret1 = dlog(sil, 1), silver_ret5 = dlog(sil, 5), gs_ratio_dev = gs - rollmeanr(gs, 60, fill = NA),
    sp_ret1 = dlog(data$SP500, 1), sp_ret5 = dlog(data$SP500, 5), sp_ret20 = dlog(data$SP500, 20),
    usd_ret1 = dlog(data$DTWEXBGS, 1), usd_ret5 = dlog(data$DTWEXBGS, 5)
  )

  # ---- technical-analysis signals (Neely et al. 2014, daily windows), each 1 or 0:
  #      moving-average rule: short MA >= long MA;  momentum rule: price >= price m days ago
  ma   <- function(k) if (k == 1) p else rollmeanr(p, k, fill = NA)
  tech <- list()
  for (s in c(1, 5, 10)) for (l in c(20, 50, 100, 200)) tech[[sprintf("tech_ma_%d_%d", s, l)]] <- as.numeric(ma(s) >= ma(l))
  for (m in c(5, 20, 60, 120)) tech[[sprintf("tech_mom_%d", m)]] <- as.numeric(p >= lag_k(p, m))
  F <- cbind(F, as.data.frame(tech))

  # ---- macro / financial / uncertainty series (levels and changes), lagged macro_lag extra days
  M <- data.frame(DAAA = data$DAAA, DBAA = data$DBAA, DGS10 = data$DGS10, T10YIE = data$T10YIE,
                  WTB3MS = data$WTB3MS, TMS = data$TMS, DFY = data$DFY, VIXCLS = data$VIXCLS,
                  EPU = log(data$USEPUINDXD),
                  d_DGS10 = c(NA, diff(data$DGS10)), d_T10YIE = c(NA, diff(data$T10YIE)),
                  d_VIX = c(NA, diff(data$VIXCLS)), d_DFY = c(NA, diff(data$DFY)))
  for (g in grep("GPR", names(data), value = TRUE)) M[[paste0("log_", g)]] <- rollmeanr(log1p(data[[g]]), 5, fill = NA)
  F <- cbind(F, as.data.frame(lapply(M, lag_k, k = macro_lag)))

  # ---- risk-free rate and variance forecast at the origin (used by the trading-rule evaluation)
  rf   <- lag_k(data$WTB3MS, 1) / 100 * h / 252
  sig2 <- h * rollapplyr(r1, vol_win, var, fill = NA)

  keep <- complete.cases(F) & !is.na(y) & !is.na(rf)
  if (any(diff(which(keep)) != 1)) stop("Usable origins are not contiguous: check for gaps in the data.")
  X <- as.matrix(F[keep, , drop = FALSE])
  stopifnot(is.numeric(X), !anyNA(X))                               # rlasso / randomForest need a numeric matrix

  md <- list(X = X, y = y[keep], date = data$date[keep], h = h, target = target, P = p[keep], gret = gret[keep],
             rf = rf[keep], sig2 = sig2[keep])
  md$val_idx  <- which(md$date >= val_start & md$date < test_start)
  md$test_idx <- which(md$date >= test_start)
  if (!length(md$val_idx) || !length(md$test_idx)) stop("Empty validation or test block: check val_start / test_start.")
  if (anyNA(md$sig2[c(md$val_idx, md$test_idx)])) stop("Variance forecast is missing in the evaluation period.")

  message(sprintf("h=%d (%s) | origins %s -> %s | p=%d | train %d | val %d | test %d",
                  h, target, md$date[1], md$date[length(md$y)], ncol(X), md$val_idx[1] - 1, length(md$val_idx), length(md$test_idx)))
  md
}

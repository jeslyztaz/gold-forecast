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
# include `date` or the target. Market series (yields, spreads, VIX) enter as of the close of day t; uncertainty indices
# (EPU, GPR) are lagged `macro_lag` extra days and CPI `cpi_lag` trading days, for their publication delays.
# Brent oil (DCOILBRENTEU), natural gas (DHHNGSP) and CPI (CPIAUCSL) are used only if their files are in data/raw.
#
# Returns a list:
#   X, y, date        one row per usable origin (X is a numeric matrix with column names)
#   P, gret           gold price at the origin, and the realised simple gold return over the next h days
#   h, target         horizon and target type
#   rf                h-day risk-free rate known at the origin (3-month T-bill, WTB3MS, lagged one day)
#   sig2              variance forecast of the h-day return (h x rolling variance of daily log returns)
#   val_idx, test_idx row positions (in X / y / date) of the validation and test origins
prepare_model_data <- function(data, h = 1, val_start, test_start, target = c("logreturn", "change"), macro_lag = 1, cpi_lag = 40, vol_win = 252) {
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

  # ---- market series (yields, spreads, VIX): closing values are known at the close of day t, so no extra lag.
  #      vix_high = 1 if VIX is above the 80th percentile of the previous 252 days (high-uncertainty dummy)
  vix_p80 <- lag_k(rollapplyr(data$VIXCLS, 252, quantile, probs = 0.8, fill = NA), 1)
  M <- data.frame(DAAA = data$DAAA, DBAA = data$DBAA, DGS10 = data$DGS10, T10YIE = data$T10YIE,
                  WTB3MS = data$WTB3MS, TMS = data$TMS, DFY = data$DFY, VIXCLS = data$VIXCLS,
                  d_DGS10 = c(NA, diff(data$DGS10)), d_T10YIE = c(NA, diff(data$T10YIE)),
                  d_VIX = c(NA, diff(data$VIXCLS)), d_DFY = c(NA, diff(data$DFY)),
                  vix_high = as.numeric(data$VIXCLS > vix_p80))
  F <- cbind(F, M)

  # ---- uncertainty indices (published with a delay), lagged macro_lag extra days
  U <- data.frame(EPU = log(data$USEPUINDXD))
  for (g in grep("GPR", names(data), value = TRUE)) U[[paste0("log_", g)]] <- rollmeanr(log1p(data[[g]]), 5, fill = NA)
  F <- cbind(F, as.data.frame(lapply(U, lag_k, k = macro_lag)))

  # ---- delayed commodity returns (Cohen & Aiche: lagged silver and gas returns carry information)
  lags <- c(1, 2, 5, 8)
  for (k in lags) F[[sprintf("silver_ret1_l%d", k)]] <- lag_k(F$silver_ret1, k)

  # ---- optional series: added only when the file is in data/raw (FRED series names)
  positive <- function(v) { if (any(data[[v]] <= 0, na.rm = TRUE)) stop(v, " has non-positive values: log returns undefined"); data[[v]] }
  if ("DCOILBRENTEU" %in% names(data)) {                            # Brent crude oil (commodity proxy for GSCI / CRB)
    oil <- positive("DCOILBRENTEU")
    F$oil_ret1 <- dlog(oil, 1); F$oil_ret5 <- dlog(oil, 5); F$oil_ret20 <- dlog(oil, 20)
  }
  if ("DHHNGSP" %in% names(data)) {                                 # Henry Hub natural gas
    gas <- positive("DHHNGSP")
    F$gas_ret1 <- dlog(gas, 1); F$gas_ret5 <- dlog(gas, 5)
    for (k in lags) F[[sprintf("gas_ret1_l%d", k)]] <- lag_k(F$gas_ret1, k)
  }
  if ("CPIAUCSL" %in% names(data)) {                                # CPI: latest month-on-month log change (Risse's cpi)
    cpi <- data$CPIAUCSL                                            # monthly value, carried forward over the month
    if (anyNA(cpi)) stop("CPIAUCSL has missing values: check that the CPI file covers the whole sample")
    new_month <- c(TRUE, diff(cpi) != 0)                            # rows where a new monthly value starts
    mom <- c(NA, diff(log(cpi[new_month])))[cumsum(new_month)]      # change vs the previous month, carried forward
    F$cpi_mom <- lag_k(mom, cpi_lag)   # CPI for month m is released ~6 weeks after the 1st of m; 40 trading days also covers the
                                       # late Sep-2025 release (24 Oct 2025). Oct-2025 was never published, so Nov-2025 is a 2-month change.
  }

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

  opt <- intersect(c("DCOILBRENTEU", "DHHNGSP", "CPIAUCSL"), names(data))
  message("Optional series used: ", if (length(opt)) paste(opt, collapse = ", ") else "none")
  message(sprintf("h=%d (%s) | origins %s -> %s | p=%d | train %d | val %d | test %d",
                  h, target, md$date[1], md$date[length(md$y)], ncol(X), md$val_idx[1] - 1, length(md$val_idx), length(md$test_idx)))
  md
}

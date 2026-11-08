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
# include `date` or the target. The gold price is a London fix, set hours before the US close, so series recorded at
# the US close (S&P 500, dollar index, yields, spreads, VIX, oil, gas) are lagged `us_lag` days; uncertainty indices
# (EPU, GPR) are lagged `macro_lag` extra days and CPI `cpi_lag` trading days, for their publication delays.
# Brent oil (DCOILBRENTEU), natural gas (DHHNGSP), CPI (CPIAUCSL) and the 20-year yield (DGS20) are used only if their
# files are in data/raw. Level series (yields, spreads, VIX, uncertainty indices, volatility) are kept in levels only if an
# ADF test on the TRAINING rows (before val_start) rejects a unit root; otherwise they enter as daily changes.
#
# Returns a list:
#   X, y, date        one row per usable origin (X is a numeric matrix with column names)
#   P, gret           gold price at the origin, and the realised simple gold return over the next h days
#   h, target         horizon and target type
#   rf                h-day risk-free rate known at the origin (3-month T-bill, WTB3MS, lagged one day)
#   sig2              variance forecast of the h-day return (h x rolling variance of daily log returns)
#   test_idx          row positions of the test origins
#   transforms        ADF result for each level series and whether it entered as a level or a change

dlog <- function(x, k = 1) {
  log(x / lag(x, n = k))
}

prepare_model_data <- function(fc_data, h = 1, test_start, target = c("logreturn", "change")) {
  target <- match.arg(target)
  fc_data <- fc_data[order(fc_data$date), ]
  p  <- fc_data$close_usd_per_troy_oz
  y <- if (target == "logreturn") lead(log(p), h) - log(p) else lead(p, h) - p
  # percentage return
  gret <- lead(p, h) / p - 1
  sil <- fc_data$silver_close_usd_per_troy_oz
  # log gold-silver ratio
  gs  <- log(p / sil)
  
  F <- data.frame(
    gold_ret1 = dlog(p), gold_ret5 = dlog(p, 5), gold_ret20 = dlog(p, 20),
    gold_dev_ma5 = log(p / fc_data$ma5), gold_dev_ma20 = log(p / fc_data$ma20),
    gold_abs_ret1 = abs(dlog(p)), RVOL_20 = fc_data$RVOL_20, RVOL_60 = fc_data$RVOL_60,
    silver_ret1 = dlog(sil), sp_ret1 = dlog(fc_data$SP500), sp_ret5 = dlog(fc_data$SP500, 5), 
    sp_ret20 = dlog(fc_data$SP500, 20), usd_ret1 = dlog(fc_data$DTWEXBGS), 
    usd_ret5 = dlog(fc_data$DTWEXBGS, 5), oil_ret1 = dlog(fc_data$DCOILBRENTEU), 
    oil_ret5 = dlog(fc_data$DCOILBRENTEU, 5), oil_ret20 = dlog(fc_data$DCOILBRENTEU, 20),
    gas_ret1 = dlog(fc_data$DHHNGSP), DAAA = fc_data$DAAA, DBAA = fc_data$DBAA, 
    DGS10 = fc_data$DGS10, T10YIE = fc_data$T10YIE, WTB3MS = fc_data$WTB3MS, TMS = fc_data$TMS, 
    DFY = fc_data$DFY, VIXCLS = fc_data$VIXCLS, d_DGS10 = c(NA, diff(fc_data$DGS10)), 
    d_T10YIE = c(NA, diff(fc_data$T10YIE)), d_VIX = c(NA, diff(fc_data$VIXCLS)), 
    d_DFY = c(NA, diff(fc_data$DFY)), EPU = log(fc_data$USEPUINDXD))
  
  # add technical indicators (Neely et al., 2014)
  # moving-average rule: short MA >= long MA;  momentum rule: price >= price m days ago
  ma <- function(k) {
    if (k == 1) {
      p
    } 
    else {
      rollmean(p, k, fill = NA, align = "right")
    }
  }
  
  tech <- list()
  
  for (s in c(1, 5, 10)) {
    for (l in c(20, 50, 100, 200)) {
      var_name <- sprintf("tech_ma_%d_%d", s, l)
      tech[[var_name]] <- as.numeric(ma(s) >= ma(l))
    }
  }
  
  for (m in c(5, 20, 60, 120)) {
    var_name <- sprintf("tech_mom_%d", m)
    tech[[var_name]] <- as.numeric(p >= lag(p, m))
  }
  
  F <- cbind(F, as.data.frame(tech))
  
  # VIX and equity volatility from Neely et al. (2014)
  # check if VIX is above the 80th percentile of the previous 252 days (high-uncertainty dummy)
  vix_p80 <- lag(
    rollapply(
      fc_data$VIXCLS, width = 252,
      FUN = function(x) {
        quantile(x, probs = 0.8, na.rm = TRUE)
      },
      align = "right",
      fill = NA))
  
  M <- data.frame(vix_high = as.numeric(fc_data$VIXCLS > vix_p80))
  
  # Log level: log1p = log(1 + x), because the oil columns are often 0 and log(0) = -Inf
  # 5-day moving average of GPR
  for (g in grep("GPR", names(fc_data), value = TRUE)) {
    M[[paste0("ma5_log_", g)]] <- rollmeanr(log1p(fc_data[[g]]), 5, fill = NA)
  }
  
  F <- cbind(F, M)
  
  # Cohen & Aiche (2023): lagged silver and gas returns carry information
  lags <- c(2, 5, 8)
  for (k in lags) {
    F[[sprintf("silver_ret1_l%d", k)]] <- lag(F$silver_ret1, k)
    F[[sprintf("gas_ret1_l%d", k)]] <- lag(F$gas_ret1, k)
  }
  
  # Neely et al. (2014) / Goyal-Welch macro variables
  # daily T-bill return
  rf_d <- fc_data$WTB3MS / 100 / 252
  # change in the T-bill rate
  F$d_TBL <- c(NA, diff(fc_data$WTB3MS))
  # S&P 500 daily log excess return
  ex_sp <- dlog(fc_data$SP500) - lag(rf_d)
  # equity volatility
  F$eq_rvol <- sqrt(pi / 2) * sqrt(252) * rollmeanr(abs(ex_sp), 252, fill = NA)
  
  # risk-free rate and variance
  rf <- lag(fc_data$WTB3MS) / 100 * h / 252
  sig2 <- h * rollapplyr(F$gold_ret1, 252, var, fill = NA)
  
  # levels vs changes: Augmented Dickey-Fuller (ADF) test on the TRAINING rows only
  # a level series with a unit root (p > 0.05) is replaced by its daily change
  # (dropped if that change is already in X).
  levels_ <- intersect(c("DAAA", "DBAA", "DGS10", "T10YIE", "WTB3MS", "TMS", "DFY", "VIXCLS", 
                         "EPU", "RVOL_20", "RVOL_60", "eq_rvol", 
                         grep("^log_.*GPR", names(F), value = TRUE)), names(F))
  train_rows <- which(fc_data$date < test_start)
  transforms <- do.call(rbind, lapply(levels_, function(v) {
    x  <- F[[v]][train_rows]; x <- x[!is.na(x)]
    pv <- suppressWarnings(adf.test(x)$p.value)
    data.frame(variable = v, adf_p = round(pv, 3), used_as = if (pv <= 0.05) "level" else "change")
  }))
  
  for (v in transforms$variable[transforms$used_as == "change"]) {
    dv <- if (startsWith(v, "log_")) sub("^log_", "d_log_", v) else paste0("d_", v)
    
    if (v == "VIXCLS") {
      dv <- "d_VIX"
    }
    if (v == "WTB3MS") {
      dv <- "d_TBL"
    }
    if (!dv %in% names(F)) {
      F[[dv]] <- c(NA, diff(F[[v]])) # first difference
    }
    
    F[[v]] <- NULL # remove level variable
  }
  
  keep <- complete.cases(F) & !is.na(y) & !is.na(rf)
  
  if (any(diff(which(keep)) != 1)) {
    stop("Usable origins are not contiguous: check for gaps in the data")
  }
  
  X <- as.matrix(F[keep, , drop = FALSE])
  stopifnot(is.numeric(X), !anyNA(X)) # rlasso() / randomForest need a numeric matrix
  md <- list(X = X, y = y[keep], date = fc_data$date[keep], h = h, target = target, P = p[keep], 
             gret = gret[keep], rf = rf[keep], sig2 = sig2[keep], transforms = transforms)
  md$test_idx <- which(md$date >= test_start)
  if (!length(md$test_idx)) stop("Empty test block: check test_start")
  if (anyNA(md$sig2[md$test_idx])) stop("Variance forecast is missing in the evaluation period.")

  message("Entered as changes (ADF on training rows): ", 
          paste(transforms$variable[transforms$used_as == "change"], collapse = ", "))
  message(sprintf("h=%d (%s) | origins %s -> %s | p=%d | train %d | test %d", h, target, 
                  md$date[1], md$date[length(md$y)], ncol(X), sum(md$date < test_start),
                  length(md$test_idx)))
  
  return(md)
}

md <- prepare_model_data(forecast_dataset, test_start = as.Date("2022-01-01"), target = "logreturn")

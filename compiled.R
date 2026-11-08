library(tidyverse)    # data manipulation
library(tools)
library(readr)
library(readxl)       # for reading excel files
library(zoo)          # for moving average
library(tseries)      # for ADF test
library(hdm)          # for rlasso()
library(randomForest) # for bagging and random forest
library(lsei)         # for forecast combination
library(sandwich)     # for metrics

bond_ret <- function(y, maturity = 20, freq = 252) {
  dates <- index(y)
  y <- as.numeric(y) / 100
  cpn <- lag(y, n = 1)
  n <- 2 * (maturity - 1 / freq)
  P <- (cpn / y) * (1 - (1 + y / 2)^(-n)) +
    (1 + y / 2)^(-n)
  result <- 100 * log(P + cpn / freq)
  xts::xts(result, order.by = dates)
}

build_dataset <- function(root) {
  paths <- sort(list.files(root, pattern = "\\.csv$", full.names = TRUE))
  
  if (length(paths) == 0L) {
    stop("No files found in ", root, call. = FALSE)
  }
  
  file_names <- basename(paths)
  clean_names <- file_path_sans_ext(file_names)
  clean_names <- gsub("-", "_", clean_names)
  
  for (i in seq_along(paths)) {
    # Dynamically creates a variable named after clean_names[i]
    assign(clean_names[i], 
           read.csv(paths[i], fileEncoding = "UTF-8-BOM", na.strings = c("", "NA", ".")))
  }
  
  for (name in clean_names) {
    df <- get(name)
    
    # standardise date formats
    date_cols <- names(df)[grepl("date", names(df), ignore.case = TRUE)]
    
    for (col_name in date_cols) {
      df[[col_name]] <- as.Date(parse_date_time(
        as.character(df[[col_name]]),
        orders = c("mdy", "dmy", "ymd")
      ))
    }
    
    if (length(date_cols) > 0) {
      df <- df %>%
        filter(!is.na(.data[[date_cols[1]]]))
    }
    
    # handle overlapping column names
    if (name == "silver_price_history") {
      df <- df %>%
        rename(silver_close_usd_per_troy_oz = close_usd_per_troy_oz)
    }
    
    # remove unnecessary columns and replace vague column names
    if (name == "SP500") {
      df <- df %>%
        select(Date, Price) %>%
        rename(SP500 = Price) %>%
        mutate(SP500 = as.numeric(gsub(",", "", SP500)))
    }
    
    assign(name, df)
  }
  
  forecast_dataset <- gold_price_history
  
  for (name in clean_names) {
    
    if (!(name %in% c("gold_price_history", "CPIAUCSL", "DJIA"))) {
      df <- get(name)
      date_cols <- names(df)[grepl("date", names(df), ignore.case = TRUE)]
      forecast_dataset <- forecast_dataset %>%
        left_join(df, by = c("date" = date_cols[1]))
    }
  }
  
  # account for availability gap in CPI
  cpi <- get("CPIAUCSL") %>%
    filter(!is.na(CPIAUCSL),
           observation_date >= as.Date("2014-01-01"),
           observation_date <= as.Date("2025-12-01")) %>%
    arrange(observation_date) %>%
    mutate(
      month_index =
        as.numeric(format(observation_date, "%Y")) * 12 +
        as.numeric(format(observation_date, "%m")),
      cpi_inflation = 100 *
        (log(CPIAUCSL) - lag(log(CPIAUCSL))) /
        (month_index - lag(month_index)),
      # Assumed availability: 15th of the following month
      release_date = as.Date(format(observation_date + 31, "%Y-%m-01")) + 14
    ) %>%
    select(release_date, cpi_inflation)
  
  # calculate DFR (default return spread)
  yd <- forecast_dataset %>%
    select(date, DBAA, DGS20) %>%
    filter(!is.na(DBAA), !is.na(DGS20)) %>%
    arrange(date)
  
  yd_xts <- xts::xts(yd[, c("DBAA", "DGS20")], order.by = yd$date)
  # BAA corporate bond return
  r_baa <- bond_ret(yd_xts[, "DBAA"], maturity = 20, freq = 252)
  # 20-year Treasury bond return
  r_t20 <- bond_ret(yd_xts[, "DGS20"], maturity = 20, freq = 252)
  # DFR = BAA return - 20-year Treasury return
  dfr_d <- r_baa - r_t20
  colnames(dfr_d) <- "dfr"
  dfr_df <- data.frame(date = as.Date(index(dfr_d)), DFR = as.numeric(dfr_d))
  
  # calculate LTR (long-term return)
  ltr_d <- r_t20
  colnames(ltr_d) <- "ltr"
  ltr_df <- data.frame(date = as.Date(index(ltr_d)), LTR = as.numeric(ltr_d))
  
  # calculate EP and DP (earnings-price ratio & dividend-price ratio)
  shiller <- read_excel(file.path(root, "ie_data.xls"), sheet = "Data", skip=7) %>%
    filter(!is.na(Date)) %>%
    mutate(
      Date = as.numeric(Date),
      E = as.numeric(E),
      D = as.numeric(D)) %>%
    filter(!is.na(Date)) %>%
    mutate(
      yr = floor(Date),
      mon = round((Date - yr) * 100),
      month = as.Date(sprintf("%d-%02d-01", yr, mon))) %>%
    arrange(month)
  
  # monthly earnings
  e_m <- xts::xts(shiller$E, order.by = shiller$month)["2013-06-01/2025-12-31"]
  colnames(e_m) <- "E12"
  # 3-month publication lag
  e_avail <- xts::lag.xts(e_m, 1)
  # monthly dividends
  ok <- !is.na(shiller$D) & shiller$D > 0
  d_m <- xts::xts(shiller$D[ok], order.by = shiller$month[ok])["2013-06-01/2025-12-31"]
  colnames(d_m) <- "D12"
  d_avail <- xts::lag.xts(d_m, 1)
  # daily S&P 500
  sp_d <- xts::xts(forecast_dataset$SP500, order.by = forecast_dataset$date)
  sp_d <- sp_d[!is.na(sp_d)]
  colnames(sp_d) <- "SP500"
  # carry forward latest available monthly earnings
  e_d <- na.locf(merge(e_avail, xts::xts(, order.by = index(sp_d))))[index(sp_d)]
  # carry forward latest available monthly dividends
  d_d <- na.locf(merge(d_avail, xts::xts(, order.by = index(sp_d))))[index(sp_d)]
  # daily EP and DP
  ep_d <- log(e_d) - log(sp_d)
  dp_d <- log(d_d) - log(sp_d)
  colnames(ep_d) <- "ep"
  colnames(dp_d) <- "dp"
  ep_df <- data.frame(date = as.Date(index(ep_d)), EP = as.numeric(ep_d))
  dp_df <- data.frame(date = as.Date(index(dp_d)), DP = as.numeric(dp_d))
  
  # calculate BM (book-to-market ratio)
  gw <- read_excel(file.path(root, "PredictorData2025.xlsx"), sheet = "Monthly")
  
  gw <- gw %>%
    mutate(month = as.Date(paste0(yyyymm, "01"), format = "%Y%m%d"))
  
  bm_m <- xts::xts(as.numeric(gw[["b/m"]]), order.by = gw$month)["2014-10-01/2025-12-31"]
  bm_m <- bm_m[!is.na(bm_m)]
  colnames(bm_m) <- "bm"
  
  dj_raw <- DJIA %>%
    mutate(Price = as.numeric(gsub(",", "", Price)), Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    filter(!is.na(Price), !is.na(Date)) %>%
    arrange(Date)
  
  dj_d <- xts::xts(dj_raw$Price, order.by = dj_raw$Date)["2014-10-01/2025-12-31"]
  colnames(dj_d) <- "djia"
  # monthly DJIA month-end closing price
  dj_mend <- dj_d[xts::endpoints(dj_d, on = "months"), ]
  index(dj_mend) <- as.Date(format(index(dj_mend), "%Y-%m-01"))
  # implied monthly book value
  bv_m <- merge(bm_m, dj_mend, join = "inner")
  bv_m <- bv_m[, "bm"] * bv_m[, "djia"]
  colnames(bv_m) <- "BV"
  # Use previous month's estimated book value
  bv_avail <- xts::lag.xts(bv_m, 1)
  # carry forward to daily DJIA dates
  bv_d <- na.locf(merge(bv_avail, xts::xts(, order.by = index(dj_d))))[index(dj_d)]
  
  # avoid indefinitely carrying forward stale book value
  last_ok <- seq(end(bv_avail), by = "month", length.out = 2)[2] - 1
  bv_d[index(bv_d) > last_ok] <- NA
  # daily BM
  bm_d <- bv_d / dj_d
  colnames(bm_d) <- "bm"
  bm_df <- data.frame(date = as.Date(index(bm_d)), BM = as.numeric(bm_d))
  
  forecast_dataset <- forecast_dataset %>%
    left_join(cpi, join_by(closest(date >= release_date))) %>%
    select(-release_date) %>%
    left_join(dfr_df, by = "date") %>%
    left_join(ltr_df, by = "date") %>%
    left_join(ep_df, by = "date") %>%
    left_join(dp_df, by = "date") %>%
    left_join(bm_df, by = "date") %>%
    arrange(date) %>%
    # only keep trading days
    filter(wday(date, week_start = 1) <= 5) %>%
    # forward-fill some data due to weekly data or holidays
    fill(everything(), .direction = "down") %>%
    # get lags and moving averages before dataset is sliced
    mutate(
      lag1 = lag(close_usd_per_troy_oz),
      lag5 = lag(close_usd_per_troy_oz, 5),
      lag20 = lag(close_usd_per_troy_oz, 20),
      ma5 = rollmean(close_usd_per_troy_oz, k = 5, fill = NA, align = "right"),
      ma20 = rollmean(close_usd_per_troy_oz, k = 20, fill = NA, align = "right"),
      .before = "GPR_AI"
    ) %>%
    # create columns for calculated variables
    mutate(
      TMS = DGS10 - WTB3MS,
      DFY = DBAA - DAAA,
      gold_return = log(close_usd_per_troy_oz / lag1),
      RVOL_20 = sqrt(pi / 2) * sqrt(252) *
        rollapply(
          abs(gold_return),
          width = 20,
          FUN = mean,
          align = "right",
          fill = NA),
      RVOL_60 = sqrt(pi / 2) * sqrt(252) *
        rollapply(
          abs(gold_return),
          width = 60,
          FUN = mean,
          align = "right",
          fill = NA)
    ) %>%
    filter(date >= "2015-01-01" & date <= "2025-12-31") # keep 10 years of data
  
  return(forecast_dataset)
}

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

get_historical_average <- function(md) {
  n <- length(md$y)
  h <- md$h
  pred <- rep(NA_real_, n)
  idx <- (h + 1):n
  pred[idx] <- cumsum(md$y)[idx - h] / (idx - h)
  return(pred_test = pred[md$test_idx])
}

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
      stopifnot(length(tr) >= min_trin)a
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
    rf_default = list(pred_val = f_rfd[seq_len(n_val)], pred_test = f_rfd[te],
                      importance = imp_rfd / n_refit, oob_mse = oob_rfd / n_refit, 
                      mtry = floor(p / 3))))
}

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

make_forecast_results <- function(md, pred_test, model) {
  stopifnot(length(pred_test) == length(md$test_idx))
  i <- md$test_idx; f <- as.numeric(pred_test); P0 <- md$P[i]
  ret_f <- if (md$target == "logreturn") exp(f) - 1 else f / P0
  
  tibble(date = md$date[i], horizon = md$h, model = model, actual = md$y[i], forecast = f,
         ret_forecast = ret_f, price_forecast = P0 * (1 + ret_f), 
         price_actual = P0 * (1 + md$gret[i]), gret = md$gret[i], rf = md$rf[i], sig2 = md$sig2[i])
}

fc_data <- build_dataset("data/raw")

horizons <- c(1, 5, 20)
test_start <- as.Date("2022-01-01")

for (h in horizons) {
  md <- prepare_model_data(fc_data, h = h, test_start = test_start, target = "logreturn")
  models <- list(LASSO = lasso, `Post-LASSO` = postlasso, Bagging = bagging, `Random Forest (tuned)` = rf)
  
  # run models
  f_ha <- get_historical_average(md)
  f_lasso <- run_rlasso(md)
  f_plasso <- run_rlasso(md)
  f_trees <- run_bagging_rf(md)
  f_ensemble <- run_ensemble(md, models, f_ha)
  
  forecast_results <- bind_rows(
    make_forecast_results(md, ha$pred_test, model = "HA"),
    make_forecast_results(md, lasso$pred_test, model = "LASSO"),
    make_forecast_results(md, postlasso$pred_test, model = "Post-LASSO"),
    make_forecast_results(md, bagging$pred_test, model = "Bagging"),
    make_forecast_results(md, rf$pred_test, model = "Random Forest (tuned)"),
    make_forecast_results(md, rf_default$pred_test, model = "Random Forest (default)"),
    imap_dfr(ensemble$pred_test, ~ make_forecast_results(md, .x, model = .y))
  )
  
  write.csv(forecast_results, "data/forecast_results.csv")
}
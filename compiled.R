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
    # add 1 year to lower bound since subsequent computations required up to 1 year of data
    filter(date >= "2014-01-01" & date <= "2025-12-31")
  
  return(forecast_dataset)
}

dlog <- function(x, k = 1) {
  log(x / lag(x, n = k))
}

prepare_model_data <- function(fc_data, h = 1, train_start, test_start, target = c("logreturn", "change")) {
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
  train_rows <- which(fc_data$date >= train_start & fc_data$date < test_start)
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
  #message("Computing historical average")
  n <- length(md$y)
  h <- md$h
  pred <- rep(NA_real_, n)
  idx <- (h + 1):n
  pred[idx] <- cumsum(md$y)[idx - h] / (idx - h)
  #message("Completed historical average calculation")
  return(list(pred_test = pred[md$test_idx]))
}

run_rlasso <- function(md, post = FALSE) {
  p <- ncol(md$X)
  ev <- md$test_idx
  f <- length(md$test_idx)
  sel <- matrix(0, f, p, dimnames = list(format(md$date[ev]), colnames(md$X)))
  
  for (i in seq_len(f)) {
    t  = ev[i]
    tr <- 1:(t - md$h)
    m <- rlasso(md$X[tr, ], md$y[tr], post = post, intercept = TRUE)
    b <- m$coefficients # (Intercept), then one per predictor
    b[is.na(b)] <- 0
    f[i] <- b[1] + sum(md$X[t, ] * b[-1]) # forecast = intercept + x_t' beta
    sel[i, ] <- as.numeric(b[-1] != 0)
    #cat("rlasso", if (post) "(post)" else "", "refit", i, "of", length(ev), "| selected:", 
    #   sum(m$index), "\n")
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
  
  oob_tune  <- sapply(mtry_grid, function(metrics) {
    rf <- randomForest(md$X[tr0, ], md$y[tr0], ntree = ntree, mtry = metrics, nodesize = nodesize)
    tail(rf$mse, 1) # OOB mean squared error
  })
  
  mtry_rf <- mtry_grid[which.min(oob_tune)]
  print(data.frame(mtry = mtry_grid, OOB_MSE = oob_tune))
  cat("Chosen mtry for RF:", mtry_rf, "(bagging uses mtry =", p, ")\n")
  
  # b. expanding-window forecasts
  f_bag <- f_rf <- f_rfd <- numeric(n_ev)
  imp_bag <- imp_rf <- imp_rfd <- setNames(numeric(p), vnames)
  oob_bag <- oob_rf <- oob_rfd <- numeric(ntree)
  n_refit <- 0
  
  for (i in seq_len(n_ev)) {
    t <- ev[i]
    if ((i - 1) %% refit_every == 0) { # refit models
      tr <- 1:(t - md$h) # only targets already realised at t
      stopifnot(length(tr) >= min_train)
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

run_ensemble <- function(md, models, ha, theta = 0.9, min_obs = 60) {
  pred_list <- lapply(names(models), function(nm) {
    as.numeric(models[[nm]]$pred_test)
  })
  Fev <- do.call(cbind, pred_list) # test forecasts, one column per model
  colnames(Fev) <- names(models)
  yev <- md$y[md$test_idx]
  stopifnot(nrow(Fev) == length(yev), !anyNA(Fev))
  k <- ncol(Fev)
  nm <- colnames(Fev)
  n_test <- length(md$test_idx)
  
  w_dmspe <- w_con <- matrix(1 / k, n_test, k, dimnames = list(NULL, nm))
  w_unc   <- matrix(NA_real_, n_test, k + 1, dimnames = list(NULL, c("Intercept", nm)))
  f_dmspe <- f_con <- f_unc <- numeric(n_test)
  
  for (j in seq_len(n_test)) {
    past <- seq_len(max(j - md$h, 0)) # forecasts whose target is realised by t
    Fi <- Fev[j, ]
    
    if (length(past) >= min_obs) {
      A <- Fev[past, , drop = FALSE]
      b <- yev[past]
      # DMSPE: weights inversely proportional to discounted past squared errors (theta = 0.9)
      phi <- colSums(theta^(max(past) - past) * (b - A)^2)
      w_dmspe[j, ] <- (1 / phi) / sum(1 / phi)
      # Granger-Ramanathan: (i) constrained, weights >= 0 summing to 1, no intercept; 
      # (ii) unrestricted OLS with intercept
      w_con[j, ] <- as.numeric(lsei(a = A, b = b, c = rep(1, k), d = 1, lower = 0))
      cf <- lm.fit(cbind(1, A), b)$coefficients 
      cf[is.na(cf)] <- 0
      w_unc[j, ] <- cf
    } 
    else {
      w_unc[j, ] <- c(0, rep(1 / k, k)) # too little history: equal weights
    }
    
    f_dmspe[j] <- sum(Fi * w_dmspe[j, ])
    f_con[j] <- sum(Fi * w_con[j, ])
    f_unc[j] <- w_unc[j, 1] + sum(Fi * w_unc[j, -1])
  }
  
  f_mean <- rowMeans(Fev)
  
  return(list(
    pred_test = list(
      "Ens: mean" = f_mean,
      "Ens: median" = apply(Fev, 1, median),
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

# out-of-sample R2 in percent (positive = better than the benchmark)
# statistical benchmark used by Risse (2019)
calculate_r2oos <- function(actual, forecast, benchmark) {
  100 * (1 - sum((actual - forecast)^2) / sum((actual - benchmark)^2))
}

# Clark-West (2007) test for nested models; one-sided H1: the model beats the benchmark.
# statistical benchmark used by Risse (2019)
calculate_cw <- function(actual, forecast, benchmark, h = 1) {
  f   <- (actual - benchmark)^2 - (actual - forecast)^2 + (benchmark - forecast)^2
  fit <- lm(f ~ 1)
  V   <- NeweyWest(fit, lag = max(h - 1, 0), prewhite = FALSE, adjust = TRUE)
  stat <- unname(coef(fit)[1] / sqrt(V[1, 1]))
  c(CW_stat = stat, CW_p = 1 - pnorm(stat))
}

# Diebold-Mariano on profits
calculate_dm <- function(d) {
  n <- length(d)
  if (sd(d) == 0) return(c(DM_stat = NA_real_, DM_p = NA_real_))
  stat <- mean(d) / sqrt(var(d) / n) * sqrt((n - 1) / n)
  c(DM_stat = stat, DM_p = 2 * pt(-abs(stat), df = n - 1))
}

# Directional accuracy: hit rate (% of periods where the forecast has the right sign) and 
# the Pesaran-Timmermann (1992) test; 
# one-sided H1: predicted signs carry information about realised signs. The test is undefined (NA) 
# when the forecast always has the same sign (e.g. the historical average); 
# compare that model's hit rate with the share of up periods
calculate_direction <- function(actual, forecast) {
  up_a <- actual > 0; up_f <- forecast > 0; n <- length(actual)
  P  <- mean(up_a == up_f); Py <- mean(up_a); Px <- mean(up_f)
  Ps <- Py * Px + (1 - Py) * (1 - Px) # hit rate expected if signs were independent
  vP  <- Ps * (1 - Ps) / n
  vPs <- ((2 * Py - 1)^2 * Px * (1 - Px) + (2 * Px - 1)^2 * Py * (1 - Py)) / n + 4 * Py * Px * (1 - Py) * (1 - Px) / n^2
  stat <- if (vP - vPs > 0) (P - Ps) / sqrt(vP - vPs) else NA_real_
  c(HitRate = 100 * P, PT_stat = stat, PT_p = 1 - pnorm(stat))
}

# long/cash strategy: long gold if forecast > 0 
# cost (0.1%) is paid on every switch between gold and cash
calculate_strategy <- function(gret, rf, pos, cost = 0.001, h = 1, periods_per_year = 252) {
  trades <- abs(diff(c(0, pos)))
  R  <- pos * gret + (1 - pos) * rf - cost * trades
  W  <- c(1, cumprod(1 + R)) # wealth path
  sd_ex <- sd(R - rf); ppy <- periods_per_year / h # holding periods per year
  list(R = R,
       metrics = c(CumRet = 100 * (W[length(W)] - 1), # cumulative return, %
                   CAGR = 100 * (W[length(W)]^(ppy / length(R)) - 1), # annualised return, %
                   AnnVol = 100 * sd(R) * sqrt(ppy), # annualised volatility, %
                   Sharpe = if (sd_ex > 0) sqrt(ppy) * mean(R - rf) / sd_ex else NA_real_,
                   MaxDrawdown  = 100 * min(W / cummax(W) - 1), # maximum drawdown, %
                   Trades = sum(trades),
                   PctInGold = 100 * mean(pos)))
}

# Risse (2019) mean-variance investor (gamma = 3):
# weight on gold = (forecast - rf) / (gamma * variance), bounded to [w_min, w_max]
# returns the annualised certainty-equivalent return in % per year
calculate_econ_metrics <- function(gret, ret_forecast, rf, sig2, h = 1, gamma = 3,
                                       w_min = -0.5, w_max = 1.5, periods_per_year = 252) {
  w  <- pmin(pmax((ret_forecast - rf) / (gamma * sig2), w_min), w_max)
  Rp <- w * gret + (1 - w) * rf
  c(CER_Risse = 100 * (periods_per_year / h) * (mean(Rp) - 0.5 * gamma * var(Rp)))
}

evaluate_models <- function(forecast_results, benchmark = "HA", cost = 0.001, gamma = 3) {
  forecast_results <- forecast_results %>% 
    arrange(model, date)
  
  obj <- readRDS(paste0("outputs/model_objects_h", h, ".rds"))
  h  <- unique(forecast_results$horizon)
  stopifnot(length(h) == 1)
  
  b  <- forecast_results %>% 
    filter(model == benchmark)
  
  if (!nrow(b)) {
    stop("Benchmark '", benchmark, "' not found in forecast_results")
  }
  
  s     <- seq(1, nrow(b), by = h)
  strat <- function(pos) calculate_strategy(b$gret[s], b$rf[s], pos, h = h)
  bench <- strat(as.numeric(b$ret_forecast[s] > 0))
  
  res <- lapply(unique(forecast_results$model), function(m) {
    d <- forecast_results %>% 
      filter(model == m)
    
    stopifnot(nrow(d) == nrow(b), all(d$date == b$date), !anyNA(d$forecast))
    st <- strat(as.numeric(d$ret_forecast[s] > 0))
    
    stats <- if (m == benchmark) {
      c(R2_OOS = NA, CW_stat = NA, CW_p = NA, DM_stat = NA, DM_p = NA)
      } 
    else {
      c(R2_OOS = calculate_r2oos(d$actual, d$forecast, b$forecast),
        calculate_cw(d$actual, d$forecast, b$forecast, h), calculate_dm(st$R - bench$R))
    }
    
    dirn <- calculate_direction(d$gret[s], d$ret_forecast[s])
    cer  <- calculate_econ_metrics(d$gret[s], d$ret_forecast[s], d$rf[s], d$sig2[s], h, gamma)
    as_tibble(as.list(c(stats, dirn, st$metrics, cer))) %>% 
      mutate(model = m, .before = 1)
  }) %>% 
    bind_rows()
  
  # reference strategies: buy and hold gold (its hit rate = share of up periods, the bar a model has to clear), buy and hold cash
  ref <- function(name, pos) {
    as_tibble(as.list(c(if (pos == 1) calculate_direction(b$gret[s], rep(1, length(s))),
                        strat(rep(pos, length(s)))$metrics))) %>% 
      mutate(model = name, .before = 1)
  }
  
  bind_rows(res, ref("Buy & hold gold", 1), ref("Buy & hold cash", 0)) %>% 
    arrange(desc(CumRet))
}

plots <- function(forecast_results, metrics) {
  
  save_plot <- function(p, name, w = 10, ht = 6) {
    ggsave(paste0("outputs/plots/", name, ".png"), p, width = w, height = ht, dpi = 150, 
           bg = "white")
  }
  
  forecast_results <- forecast_results %>% 
    mutate(model = factor(model, levels = unique(model)))
  
  obj <- readRDS(paste0("outputs/model_objects_h", h, ".rds"))
  theme_set(theme_minimal(base_size = 11))
  
  # actual vs predicted gold price (price at t+h) over the last 90 days
  p1 <- forecast_results %>% 
    filter(date > max(date) - 90) %>%
    ggplot(aes(date)) +
    geom_line(aes(y = price_actual, colour = "Actual"), linewidth = 0.4) +
    geom_line(aes(y = price_forecast, colour = "Predicted"), linewidth = 0.4) +
    facet_wrap(~ model, ncol = 2) +
    scale_colour_manual(values = c(Actual = "grey55", Predicted = "#1f77b4"), name = NULL) +
    labs(
      title = sprintf("Actual vs. Predicted gold price (%d-day ahead, last 90 days of the test period)", h),
      x = "Forecast origin", y = "USD per troy oz") + theme(legend.position = "top")
  
  save_plot(p1, paste0("h", h, "_1_actual_vs_predicted_price"), ht = 8)
  
  # actual vs predicted target
  p2 <- forecast_results %>%
    ggplot(aes(forecast, actual)) +
    geom_point(alpha = 0.25, size = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "red") +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.6, formula = y ~ x) +
    facet_wrap(~ model, ncol = 3, scales = "free_x") +
    labs(title = paste0("Predicted vs. Realised Log Returns (Test Period), h = ", h), 
         x = "Predicted", y = "Actual")
  
  save_plot(p2, paste0("h", h, "_2_actual_vs_predicted_scatter"), ht = 7)
  
  # cumulative squared-error difference vs. historical average
  ha  <- forecast_results %>% 
    filter(model == "HA") %>% 
    select(date, ha_forecast = forecast)
  
  p3 <- forecast_results %>% 
    filter(model != "HA") %>% 
    left_join(ha, by = "date") %>%
    group_by(model) %>% 
    arrange(date) %>%
    mutate(csfe = cumsum((actual - ha_forecast)^2 - (actual - forecast)^2)) %>%
    ggplot(aes(date, csfe, colour = model)) +
    geom_hline(yintercept = 0, colour = "grey50") + geom_line(linewidth = 0.6) +
    labs(title = paste0("Cumulative Squared-Error Difference vs. Historical Average, h = ", h),
         x = NULL, y = "Cumulative (e_HA^2 - e_model^2)", colour = NULL) + 
    theme(legend.position = "top")
  
  save_plot(p3, paste0("h", h, "_3_cumulative_error_difference"))
  
  # out-of-sample R^2 vs the historical average (stars: Clark-West test, * 10%, ** 5%, *** 1%)
  p4 <- metrics %>% 
    filter(!is.na(R2_OOS)) %>%
    mutate(stars = cut(CW_p, c(-Inf, 0.01, 0.05, 0.10, Inf), labels = c("***", "**", "*", "")),
           model = reorder(model, R2_OOS)) %>%
    ggplot(aes(R2_OOS, model, fill = R2_OOS > 0)) +
    geom_col(show.legend = FALSE) + 
    geom_text(aes(label = stars), hjust = -0.2) +
    scale_fill_manual(values = c(`TRUE` = "#2ca02c", `FALSE` = "#d62728")) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(title = paste0("Out-of-Sample R^2 vs. Historical Average (%), h = ", h), 
         x = "R2_OOS (%)", y = NULL)
  
  save_plot(p4, paste0("h", h, "_4_r2_oos"), ht = 4.5)
  
  # trading strategy: growth of 1 invested
  b <- forecast_results %>% 
    filter(model == "HA") %>% 
    arrange(date)
  
  s <- seq(1, nrow(b), by = h)
  
  wealth <- function(name, pos) {
    tibble(model = name, date = b$date[s],
           wealth = cumprod(1 + calculate_strategy(b$gret[s], b$rf[s], pos, h = h)$R))
  }
  
  ref <- bind_rows(wealth("Buy & hold gold", 
                          rep(1, length(s))), wealth("Buy & hold cash", rep(0, length(s))))
  
  p5 <- bind_rows(lapply(
    setdiff(unique(as.character(forecast_results$model)), 
            c("HA", "Buy & hold gold", "Buy & hold cash")),
    function(m) {
      d <- forecast_results %>%
        filter(as.character(model) == m) %>%
        arrange(date)
      
      wealth(m, as.numeric(d$ret_forecast[s] > 0))
      } )) %>%
    ggplot(aes(date, wealth)) +
    geom_line(data = ref, aes(linetype = model), colour = "black", linewidth = 1.1, alpha = 0.35) +
    geom_line(aes(colour = model), linewidth = 0.6) +
    labs(title = paste0("Long/Cash Strategy, 0.1% Cost per Switch, h = ", h),
         subtitle = "Models that take the same positions as buy-and-hold gold lie on top of its line",
         x = NULL, y = "Value of 1 invested", colour = NULL, linetype = NULL)
  
  save_plot(p5, paste0("h", h, "_5_strategy_wealth"))
  
  # bagging and random forest: out-of-bag error vs number of trees (average over the refits)
  p6 <- bind_rows(
    tibble(model = "Bagging", trees = seq_along(obj$bagging$oob_mse), oob = obj$bagging$oob_mse),
    tibble(model = "Random Forest", trees = seq_along(obj$rf$oob_mse), oob = obj$rf$oob_mse)) %>%
    ggplot(aes(trees, oob, colour = model)) + geom_line(linewidth = 0.7) +
    labs(
      title = paste0("Out-of-Bag MSE vs. Number of Trees, h = ", h), 
      x = "Number of Trees", 
      y = "OOB MSE (Target Units Squared)", colour = NULL)
  save_plot(p6, paste0("h", h, "_6_oob_vs_trees"), ht = 4.5)
  
  # predictor importance in the forests (IncNodePurity, share of the total, top 15)
  p7 <- bind_rows(
    tibble(model = "Bagging", var = names(obj$bagging$importance), imp = obj$bagging$importance),
    tibble(model = "Random Forest", var = names(obj$rf$importance), imp = obj$rf$importance)) %>%
    group_by(model) %>% 
    mutate(share = 100 * imp / sum(imp)) %>% 
    slice_max(share, n = 15) %>% 
    ungroup() %>%
    mutate(key = reorder(paste(model, var, sep = "___"), share)) %>%
    ggplot(aes(share, key)) + 
    geom_col(fill = "#1f77b4") +
    scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
    facet_wrap(~ model, scales = "free_y") +
    labs(title = paste0("Predictor importance (top 15), h = ", h), 
         x = "% of total node-purity increase", y = NULL)
  
  save_plot(p7, paste0("h", h, "_7_predictor_importance"), ht = 6)
  
  # LASSO / post-LASSO: how often each predictor is selected
  sel_freq <- function(m, name) { 
    f <- colMeans(m$selected)
    tibble(model = name, var = names(f), freq = 100 * f) 
  }
  
  sf <- bind_rows(sel_freq(obj$lasso, "LASSO"), sel_freq(obj$postlasso, "Post-LASSO")) %>% 
    filter(freq > 0)
  
  p8 <- if (nrow(sf) == 0) {
    ggplot() + annotate("text", x = 0, y = 0, label = "rlasso selected no predictors in any refit\n(forecast = intercept = historical average)") +
      theme_void() + labs(title = "LASSO selection frequency")
  } 
  else {
    sf %>% 
      group_by(model) %>% 
      slice_max(freq, n = 20, with_ties = FALSE) %>% 
      ungroup() %>%
      mutate(key = reorder(paste(model, var, sep = "___"), freq)) %>%
      ggplot(aes(freq, key)) + 
      geom_col(fill = "#ff7f0e") +
      scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
      facet_wrap(~ model, scales = "free_y") +
      labs(title = paste0("LASSO Selection Frequency (Top 20), h = ", h), 
           x = "% of Refits in which the Predictor is Selected", y = NULL)
  }
  
  save_plot(p8, paste0("h", h, "_8_lasso_selection_frequency"), ht = 6)
  
  # LASSO / post-LASSO: number of predictors selected at each refit
  p9 <- bind_rows(
    tibble(model = "LASSO", date = as.Date(rownames(obj$lasso$selected)), 
           n = rowSums(obj$lasso$selected)),
    tibble(model = "Post-LASSO", date = as.Date(rownames(obj$postlasso$selected)), 
           n = rowSums(obj$postlasso$selected))) %>%
    ggplot(aes(date, n, colour = model)) + geom_step(linewidth = 0.6) +
    labs(title = paste0("Number of Predictors Selected at Each Refit, h = ", h), 
         x = NULL, y = "Selected Predictors", colour = NULL)
  
  save_plot(p9, paste0("h", h, "_9_lasso_n_selected"), ht = 4)
  
  # directional accuracy: hit rate vs. the share of up periods (= hit rate of always predicting "up")
  up <- metrics$HitRate[metrics$model == "Buy & hold gold"]
  
  p10 <- metrics %>% 
    filter(!is.na(HitRate), !model %in% c("Buy & hold gold", "Buy & hold cash")) %>%
    mutate(stars = cut(PT_p, c(-Inf, 0.01, 0.05, 0.10, Inf), labels = c("***", "**", "*", "")),
           model = reorder(model, HitRate)) %>%
    ggplot(aes(HitRate, model)) +
    geom_col(fill = "#1f77b4") + 
    geom_text(aes(label = stars), hjust = -0.2) +
    geom_vline(xintercept = up, linetype = "dashed", colour = "red") +
    annotate("text", x = up, y = 0.6, label = sprintf(" up periods: %.1f%%", up), 
             hjust = 0, colour = "red", size = 3.2) +
    coord_cartesian(xlim = c(40, max(60, up + 5))) +
    labs(title = paste0("Directional Accuracy (% of Periods with the Correct Sign), h = ", h),
         subtitle = "Stars: Pesaran-Timmermann test (* 10%, ** 5%, *** 1%); dashed line = always predicting 'up'",
         x = "Hit Rate (%)", y = NULL)
  
  save_plot(p10, paste0("h", h, "_10_directional_accuracy"), ht = 4.5)
  
  # recursive combination weights over the test period
  w <- obj$ensemble$weights
  p11 <- bind_rows(
    as_tibble(w$dmspe) %>% 
      mutate(date = w$date, method = "DMSPE"),
    as_tibble(w$gr_constrained) %>% 
      mutate(date = w$date, method = "GR constrained (>= 0, sum to 1)"),
    as_tibble(w$gr_unconstrained[, -1, drop = FALSE]) %>% 
      mutate(date = w$date, method = "GR unconstrained (slopes; intercept not shown)")) %>%
    pivot_longer(-c(date, method), names_to = "model", values_to = "weight") %>%
    ggplot(aes(date, weight, colour = model)) +
    geom_hline(yintercept = 0, colour = "grey70") + 
    geom_line(linewidth = 0.6) +
    facet_wrap(~ method, ncol = 1, scales = "free_y") +
    labs(title = paste0("Combination weights at each test origin, h = ", h), 
         x = NULL, y = "Weight", colour = NULL) + 
    theme(legend.position = "top")
  save_plot(p11, paste0("h", h, "_11_combination_weights"), ht = 8)
  
  message("Saved 11 plots to ", "outputs/plots/")
}

fc_data <- build_dataset("data/raw/")

horizons <- c(1, 5, 20)
train_start <- as.Date("2015-01-01")
test_start <- as.Date("2022-01-01")

for (h in horizons) {
  md <- prepare_model_data(fc_data, h = h, train_start = train_start, test_start = test_start, 
                           target = "logreturn")
  
  # run models
  f_ha <- get_historical_average(md)
  f_lasso <- run_rlasso(md)
  f_plasso <- run_rlasso(md, post = TRUE)
  f_trees <- run_bagging_rf(md)
  f_bagging <- f_trees$bagging
  f_rf <- f_trees$rf
  f_rf_default <- f_trees$rf_default
  models <- list(LASSO = f_lasso, `Post-LASSO` = f_plasso, Bagging = f_bagging, 
                 `Random Forest (tuned)` = f_rf)
  f_ensemble <- run_ensemble(md, models, f_ha)
  
  forecast_results <- bind_rows(
    make_forecast_results(md, f_ha$pred_test, model = "HA"),
    make_forecast_results(md, f_lasso$pred_test, model = "LASSO"),
    make_forecast_results(md, f_plasso$pred_test, model = "Post-LASSO"),
    make_forecast_results(md, f_bagging$pred_test, model = "Bagging"),
    make_forecast_results(md, f_rf$pred_test, model = "Random Forest (tuned)"),
    make_forecast_results(md, f_rf_default$pred_test, model = "Random Forest (default)"),
    imap_dfr(f_ensemble$pred_test, ~ make_forecast_results(md, .x, model = .y))
  )
  
  metrics <- evaluate_models(forecast_results, benchmark = "HA")
  
  write.csv(forecast_results, paste0("outputs/forecast_results_h", h, ".csv"), row.names = FALSE)
  write.csv(metrics, paste0("outputs/metrics_h", h, ".csv"), row.names = FALSE)
  write_csv(md$transforms, paste0("outputs/transforms_h", h, ".csv"))
  saveRDS(list(lasso = f_lasso, postlasso = f_plasso, bagging = f_bagging, rf = f_rf, 
               ensemble = f_ensemble),
          paste0("outputs/model_objects_h", h, ".rds"))
  
  plots(forecast_results, metrics)
}

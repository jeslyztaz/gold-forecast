bond_ret <- function(y, maturity = 20, freq = 252) {
  dates <- zoo::index(y)
  y <- as.numeric(y) / 100
  cpn <- dplyr::lag(y, n = 1)
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
        (log(CPIAUCSL) - dplyr::lag(log(CPIAUCSL))) /
        (month_index - dplyr::lag(month_index)),
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
  shiller <- readxl::read_excel(file.path(root, "ie_data.xls"), sheet = "Data", skip=7) %>%
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
  gw <- readxl::read_excel(file.path(root, "PredictorData2025.xlsx"), sheet = "Monthly")
  
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
      lag1 = dplyr::lag(close_usd_per_troy_oz),
      lag5 = dplyr::lag(close_usd_per_troy_oz, 5),
      lag20 = dplyr::lag(close_usd_per_troy_oz, 20),
      ma5 = zoo::rollmean(close_usd_per_troy_oz, k = 5, fill = NA, align = "right"),
      ma20 = zoo::rollmean(close_usd_per_troy_oz, k = 20, fill = NA, align = "right"),
      .before = "GPR_AI"
    ) %>%
    # create columns for calculated variables
    mutate(
      TMS = DGS10 - WTB3MS,
      DFY = DBAA - DAAA,
      gold_return = log(close_usd_per_troy_oz / lag1),
      #log_vix = log(VIXCLS),
      #ty_yield = DGS10 - dplyr::lag(DGS10),
      #bei_chg = T10YIE - dplyr::lag(T10YIE),
      #epu_chg = log(USEPUINDXD / dplyr::lag(USEPUINDXD)),
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
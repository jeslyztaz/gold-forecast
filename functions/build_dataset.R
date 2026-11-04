build_dataset <- function(root) {
  
  paths <- sort(list.files(
    root,
    pattern = "\\.csv$",
    full.names = TRUE
  ))
  
  if (length(paths) == 0L) {
    stop("No files found in ", root, call. = FALSE)
  }
  
  file_names <- basename(paths)
  clean_names <- file_path_sans_ext(file_names)
  clean_names <- gsub("-", "_", clean_names)
  
  for (i in seq_along(paths)) {
    # Dynamically creates a variable named after clean_names[i]
    assign(clean_names[i], read.csv(paths[i], fileEncoding = "UTF-8-BOM", na.strings = c("", "NA", ".")))  # SP500.csv starts with a BOM; FRED may use "." for missing
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
    
    if (name == "silver_price_history") {
      df <- df %>%
        rename(silver_close_usd_per_troy_oz = close_usd_per_troy_oz)
    }
    
    if (name == "SP500") {
      df <- df %>%
        select(Date, Price) %>%
        rename(SP500 = Price) %>%
        mutate(SP500 = as.numeric(gsub(",", "", SP500)))
    }
    
    assign(name, df)
  }
  
  forecast_dataset <- gold_price_history %>%
    arrange(date) %>%
    # only keep trading days
    filter(wday(date, week_start = 1) <= 5) %>%
    # get lags and moving averages before dataset is slices
    mutate(
      lag1 = lag(close_usd_per_troy_oz),
      lag5 = lag(close_usd_per_troy_oz, n = 5),
      lag20 = lag(close_usd_per_troy_oz, n = 20),
      ma5 = rollmean(close_usd_per_troy_oz, k = 5, fill = NA, align = "right"),
      ma20 = rollmean(close_usd_per_troy_oz, k = 20, fill = NA, align = "right")
    )
  
  for (name in clean_names) {
    
    if (name != "gold_price_history") {
      df <- get(name)
      date_cols <- names(df)[grepl("date", names(df), ignore.case = TRUE)]
      # take the latest observation on or before each trading day (an exact-date join would drop e.g. a monthly
      # CPI value dated on a weekend); for daily series this gives the same result as the exact join + fill below
      df <- df[!is.na(df[[date_cols[1]]]), ]                     # drop rows whose date did not parse
      df <- df[order(df[[date_cols[1]]]), ]
      i  <- findInterval(as.numeric(forecast_dataset$date), as.numeric(df[[date_cols[1]]]))
      i[i == 0] <- NA
      forecast_dataset <- bind_cols(forecast_dataset, df[i, setdiff(names(df), date_cols), drop = FALSE])
    }
  }
  
  forecast_dataset <- forecast_dataset %>%
    # forward-fill everything as t-bills are weekly data
    # and most of other missing data are from holidays
    fill(everything(), .direction = "down") %>%
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
      # RVOL_252 = sqrt(pi / 2) * sqrt(252) *
      #   rollapply(
      #     abs(gold_return),
      #     width = 252,
      #     FUN = mean,
      #     align = "right",
      #     fill = NA)
      # missing: LTR, EP, DP, BM
    ) %>%
    filter(date >= "2015-01-01" & date <= "2025-12-31") # keep 10 years of data
}
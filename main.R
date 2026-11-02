source("setup.R")
source(here("functions", "build_dataset.R"))
source(here("functions", "get_historical_average.R"))
source(here("functions", "run_rolling_rlasso.R"))

# create forecast dataset
data <- build_dataset(here("data", "raw"))
out <- here("data", "forecast_dataset.csv")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
write_csv(data, out)
message("Wrote ", nrow(data), " rows to ", out)

# compute historical average
ha <- get_historical_average(data)
message("Computed ", nrow(ha), " rows of historical average")

# assign 6 years for training, 2 years for validation and the rest for test
val_start  <- as.Date("2021-01-01")
test_start <- as.Date("2023-01-01")

# run rlasso() with rolling window (both LASSO and post-LASSO)
lasso <- run_rolling_rlasso(data, test_start)
message("Ran rolling window LASSO")
postlasso <- run_rolling_rlasso(data, test_start, pl=TRUE)
message("Ran rolling window post-LASSO")

message("Gold forecast completed successfully.")
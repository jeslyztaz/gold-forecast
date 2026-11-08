source("setup.R")
source(here("functions", "build_dataset.R"))
source(here("functions", "prepare_model_data.R"))
source(here("functions", "get_historical_average.R"))
source(here("functions", "run_rlasso.R"))
source(here("functions", "run_bagging_rf.R"))
source(here("functions", "run_ensemble.R"))
source(here("functions", "build_results_datasets.R"))
source(here("functions", "compute_metrics.R"))

# create forecast dataset
data <- build_dataset(here("data", "raw"))
out <- here("data", "forecast_dataset.csv")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
write_csv(data, out)
message("Wrote ", nrow(data), " rows to ", out)

# assign 7 years for training, 3 years for test
test_start <- as.Date("2022-01-01")
md <- prepare_model_data(fc_data, test_start = test_start, target = "logreturn")
# md_h5 <- prepare_model_data(fc_data, h = 5, test_start = test_start, target = "logreturn")
# md_h20 <- prepare_model_data(fc_data, h = 20, test_start = test_start, target = "logreturn")

# compute historical average
ha <- get_historical_average(data)
message("Computed ", nrow(ha), " rows of historical average")

# run rlasso() with rolling window (both LASSO and post-LASSO)
lasso <- run_rlasso(data)
message("Ran LASSO")
postlasso <- run_rolling_rlasso(data, pl=TRUE)
message("Ran post-LASSO")

# run bagging + random forest
trees <- run_bagging_rf(md)
message("Ran bagging + random forest")

# run ensemble methods
models   <- list(LASSO = lasso, `Post-LASSO` = postlasso, Bagging = bagging, 
                 `Random Forest (tuned)` = rf)
ensemble <- run_ensemble(md, models, ha)

# create consolidated dataframe for ensemble & metrics
forecast_results <- bind_rows(
  make_forecast_results(md, ha$pred_test, model = "HA"),
  make_forecast_results(md, lasso$pred_test, model = "LASSO"),
  make_forecast_results(md, postlasso$pred_test, model = "Post-LASSO"),
  make_forecast_results(md, bagging$pred_test, model = "Bagging"),
  make_forecast_results(md, rf$pred_test, model = "Random Forest (tuned)"),
  make_forecast_results(md, rf_default$pred_test, model = "Random Forest (default)"),
  imap_dfr(ensemble$pred_test, ~ make_forecast_results(md, .x, model = .y))
)

out2 <- here("data", "forecast_results.csv")
dir.create(dirname(out2), recursive = TRUE, showWarnings = FALSE)
write_csv(forecast_results, out2)
message("Wrote ", nrow(forecast_results), " rows to ", out2)

message("Gold forecast completed successfully.")
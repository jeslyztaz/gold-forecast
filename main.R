source("setup.R")
source(here("functions", "build_dataset.R"))
source(here("functions", "prepare_model_data.R"))
source(here("functions", "get_historical_average.R"))
source(here("functions", "run_rolling_rlasso.R"))
source(here("functions", "run_bagging_rf.R"))
source(here("functions", "run_ensemble.R"))
source(here("functions", "build_results_datasets.R"))
source(here("functions", "compute_metrics.R"))

# ---- settings
h          <- 1                          # forecast horizon in trading days (1 = daily, 5 = weekly, 20 = monthly)
target     <- "logreturn"                # "logreturn" = log(P[t+h]/P[t]) | "change" = P[t+h] - P[t] (dollars)
cost       <- 0.001                      # transaction cost per switch between gold and cash (0.1%)
val_start  <- as.Date("2021-01-01")      # train: before val_start | validation: 2021-2022 | test: from test_start
test_start <- as.Date("2023-01-01")

# ---- 1. raw dataset (levels), then the model data: predictors X and the h-step target y at each origin
data <- build_dataset(here("data", "raw"))
out <- here("data", "forecast_dataset.csv")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
write_csv(data, out)
message("Wrote ", nrow(data), " rows to ", out)

md <- prepare_model_data(data, h = h, val_start = val_start, test_start = test_start, target = target)

# ---- 2. models. Every function returns list(pred_val, pred_test) for the validation and test origins
ha        <- get_historical_average(md)
lasso     <- run_rolling_rlasso(md, post = FALSE)
postlasso <- run_rolling_rlasso(md, post = TRUE)
bagging   <- run_bagging(md)
rf        <- run_rf(md)

models <- list(LASSO = lasso, `Post-LASSO` = postlasso, Bagging = bagging, `Random Forest` = rf)

# ---- 3. combinations: equal-weight average, and Granger-Ramanathan weights fitted on the validation block
avg_pred <- rowMeans(sapply(models, function(m) m$pred_test))
ensemble <- run_ensemble(md, models)

# ---- 4. one long table of test forecasts (HA is the benchmark) and the metrics
forecast_results <- bind_rows(
  make_forecast_results(md, ha$pred_test, model = "HA"),
  imap_dfr(models, ~ make_forecast_results(md, .x$pred_test, model = .y)),
  make_forecast_results(md, avg_pred, model = "Avg Prediction"),
  imap_dfr(ensemble$pred_test, ~ make_forecast_results(md, .x, model = .y))
)

metrics <- evaluate_models(forecast_results, benchmark = "HA", cost = cost)

write_csv(forecast_results, output_file("forecast_results.csv"))
write_csv(metrics, output_file("metrics.csv"))
saveRDS(list(lasso = lasso, postlasso = postlasso, bagging = bagging, rf = rf), output_file("model_objects.rds"))  # for plots.R
print(metrics %>% mutate(across(where(is.numeric), ~ round(.x, 3))), width = Inf)
message("Gold forecast completed successfully.")

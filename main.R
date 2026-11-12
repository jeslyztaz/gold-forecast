source("setup.R")
source(here("functions", "build_dataset.R"))
source(here("functions", "prepare_model_data.R"))
source(here("functions", "get_historical_average.R"))
source(here("functions", "run_rlasso.R"))
source(here("functions", "run_bagging_rf.R"))
source(here("functions", "run_ensemble.R"))
source(here("functions", "build_results_datasets.R"))
source(here("functions", "compute_metrics.R"))
source(here("functions", "create_plots.R"))

fc_data <- build_dataset(here("data", "raw"))
out <- here("data", "forecast_dataset.csv")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
write_csv(fc_data, out)
message("Wrote ", nrow(data), " rows to ", out)

horizons <- c(1, 5, 20)
train_start <- as.Date("2015-01-01")
test_start <- as.Date("2022-01-01")

for (h in horizons) {
  md <- prepare_model_data(fc_data, h = h, train_start = train_start, test_start = test_start, 
                           target = "logreturn")
  
  # run models
  ha <- get_historical_average(md)
  lasso <- run_rlasso(md)
  plasso <- run_rlasso(md, post = TRUE)
  trees <- run_bagging_rf(md)
  bagging <- trees$bagging
  rf <- trees$rf
  rf_default <- trees$rf_default
  models <- list(LASSO = lasso, `Post-LASSO` = plasso, Bagging = bagging, 
                 `Random Forest (Tuned)` = rf)
  ensemble <- run_ensemble(md, models, ha)
  
  forecast_results <- bind_rows(
    make_forecast_results(md, ha$pred_test, model = "HA"),
    make_forecast_results(md, lasso$pred_test, model = "LASSO"),
    make_forecast_results(md, plasso$pred_test, model = "Post-LASSO"),
    make_forecast_results(md, bagging$pred_test, model = "Bagging"),
    make_forecast_results(md, rf$pred_test, model = "Random Forest (Tuned)"),
    make_forecast_results(md, rf_default$pred_test, model = "Random Forest (Default)"),
    imap_dfr(ensemble$pred_test, ~ make_forecast_results(md, .x, model = .y))
  )
  
  out2 <- here("outputs", paste0("model_objects_h", h, ".rds"))
  saveRDS(list(lasso = lasso, postlasso = plasso, bagging = bagging, rf = rf, 
               ensemble = ensemble), out2)
  message("Saved model RDS to ", out)
  
  metrics <- evaluate_models(forecast_results, benchmark = "HA")
  
  out3 <- here("outputs", paste0("outputs/forecast_results_h", h, ".csv"))
  dir.create(dirname(out3), recursive = TRUE, showWarnings = FALSE)
  write.csv(forecast_results, out3, row.names = FALSE)
  
  out4 <- here("outputs", paste0("outputs/metrics_h", h, ".csv"))
  dir.create(dirname(out4), recursive = TRUE, showWarnings = FALSE)
  write.csv(metrics, out4, row.names = FALSE)
  
  out5 <- here("outputs", paste0("outputs/transforms_h", h, ".csv"))
  dir.create(dirname(out5), recursive = TRUE, showWarnings = FALSE)
  write_csv(md$transforms, out5)
  
  plots(forecast_results, metrics)
}

message("Gold forecast completed successfully.")
## Project Structure

```
gold-forecast/
├── data/
│   ├── raw/                         # raw series (FRED, GPR, gold, silver, S&P 500)
│   └── forecast_dataset.csv         # written by main.R (levels, one row per trading day)
├── functions/
│   ├── build_dataset.R              # raw csv files  -> forecast_dataset (levels)
│   ├── prepare_model_data.R         # dataset -> md: predictors X, h-step target y, rf, sig2, val/test indices
│   ├── get_historical_average.R     # HA benchmark                        -> list(pred_val, pred_test)
│   ├── run_rolling_rlasso.R         # LASSO / post-LASSO (hdm::rlasso)    -> list(pred_val, pred_test, n_selected)
│   ├── run_bagging_rf.R             # bagging and random forest           -> list(pred_val, pred_test)
│   ├── run_ensemble.R               # Granger-Ramanathan combinations     -> list(pred_test, weights)
│   ├── build_results_datasets.R     # long table of test forecasts (date, model, actual, forecast, rf, sig2)
│   └── compute_metrics.R            # long/cash strategy (return, Sharpe, max drawdown, 0.1% costs), DM on profits, R2_OOS, CW, Risse CER
├── notebooks/                       # exploratory only (superseded by functions/ + main.R)
├── outputs/                         # forecast_results.csv, metrics.csv (written by main.R)
├── main.R
├── README.md
└── setup.R
```

Our project results can be replicated by running `main.R`.

### Conventions (shared by every model)
* A row is a forecast **origin** t (close of day t). The target (`target` in `main.R`) is either `log(P_{t+h}/P_t)` or the dollar change `P_{t+h} - P_t`; the price forecast is backed out in `forecast_results`.
* A model fitted at origin t only uses rows `s <= t - h` (targets already realised).
* Predictors include Neely et al. (2014) technical-analysis signals. They are stationary transformations (returns, deviations, spreads); `date` and price levels are never predictors.
* Every model returns `list(pred_val, pred_test)`, aligned with the validation / test origins in `md`.
* Horizon `h`, `val_start` and `test_start` are set once at the top of `main.R`.

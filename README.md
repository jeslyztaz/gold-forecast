## Project Structure

```
gold-forecast/
├── data/
│   ├── raw/
│   ├── forecast_dataset.csv
│   └── README.md                    # raw data description and sources
├── functions/
│   ├── build_dataset.R
│   ├── build_results_dataset.R
│   ├── compute_metrics.R
│   ├── create_plots.R
│   ├── get_historical_average.R
│   ├── prepare_model_data.R
│   ├── run_bagging_rf.R
│   ├── run_ensemble.R
│   └── run_rlasso.R
├── notebooks/
├── outputs/
│   ├── plots/
│   ├── forecast_results_hX.R        # X = 1, 5 or 20
│   ├── metrics_hX.csv
│   ├── model_objects_hX.rds
│   └── transforms.csv               # only 1 version since it's the same for all horizons
├── compiled.R                       # all functions in 1 script
├── main.R
├── README.md
└── setup.R
```

Our project results can be replicated by running `main.R` or `compiled.R`.
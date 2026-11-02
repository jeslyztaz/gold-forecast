## Project Structure

```
gold-forecast/
├── data/
│   ├── raw/
│   ├── forecast_dataset.csv
│   └── README.md                    # raw data description and sources
├── functions/
│   ├── build_dataset.R
│   ├── get_historical_average.R
│   ├── run_bagging_rf.R
│   ├── run_rolling_lasso.R
│   └── metrics.R
├── notebooks/
├── main.R
├── README.md
└── setup.R
```

Our project results can be replicated by running `main.R`
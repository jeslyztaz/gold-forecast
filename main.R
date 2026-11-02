source("setup.R")
source(here("functions", "build_dataset.R"))

# create forecast dataset
data <- build_dataset(here("data", "raw"))
out <- here("data", "forecast_dataset.csv")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
write_csv(data, out)
message("Wrote ", nrow(data), " rows to ", out)

message("Gold forecast completed successfully.")
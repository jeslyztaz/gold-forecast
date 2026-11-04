library(tidyverse)    # data manipulation (also loads lubridate, readr)
library(zoo)          # rollmean(), rollapply() -- used by build_dataset() and prepare_model_data()
library(tools)        # file_path_sans_ext()
library(hdm)          # for rlasso()
library(randomForest) # for bagging and random forest
library(lsei)         # for forecast combination
library(sandwich)     # for metrics
library(here)         # project-relative file paths

# Path under outputs/. The parent directory is created when a file is saved.
output_file <- function(...) {
  path <- here("outputs", ...)
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

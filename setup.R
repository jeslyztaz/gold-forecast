library(tidyverse)    # data manipulation
library(tools)
library(readr)
library(hdm)          # for rlasso()
library(randomForest) # for bagging and random forest
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
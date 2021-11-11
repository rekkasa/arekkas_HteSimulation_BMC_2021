#!/usr/bin/env Rscript

# =============================================================
# Description:
#   Creates a csv file with all median and the
#   0.025 and 0.975 quantiles of the
#   RMSE/AUC/calibration sampling distribution
# Input:
#   The metric we are interested in:
#     - rmse
#     - auc
#     - calibration
# Output:
#   - data/processed/(rmse|auc|calibration)Distribution.csv
# Depends:
#   - code/helpers/GetMedians.R
# =============================================================

args <- commandArgs(trailingOnly = TRUE)
args_metric <- as.character(args[1])
saveFile <- paste0(
  args_metric,
  "Distribution.csv"
)

library(tidyverse)
library(readr)

source("code/helpers/GetMedians.R")

read_csv("data/processed/rmse.csv") %>%
  group_by(scenarioId) %>%
  arrange(scenarioId) %>%
  nest() %>%
  mutate(
    median = map(
      data,
      ~ getMedian(
        data = .x
      )
    ),
    lower = map(
      data,
      ~ getLower(
        data = .x
      )
    ),
    upper = map(
      data,
      ~ getUpper(
        data = .x
      )
    )
  ) %>%
  ungroup() %>%
  select(-data) %>%
  unnest(c("median", "lower", "upper")) %>%
  write_csv(
    file.path(
      "data/processed",
      saveFile
    )
  )

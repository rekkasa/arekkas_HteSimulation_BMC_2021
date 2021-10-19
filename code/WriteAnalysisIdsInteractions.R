#!/usr/bin/env Rscript

library(tidyverse)
library(readr)

base <- "interaction"
type <- c("weak", "strong", "mixed")
n <- 4250
auc <- .75

table <- tibble(
  base       = base,
  type       = type,
  sampleSize = n,
  auc        = auc
) %>%
  mutate(
    b0 = -2.08,
    b1 = .49,
    b2 = .49,
    b3 = .49,
    b4 = .49,
    b5 = .49,
    b6 = .49,
    b7 = .49,
    b8 = .49,
    g0 = log(.8),
    g1 = case_when(
      (type == "weak" | type == "mixed") ~ -.19,
      (type == "strong"                  ) ~ -.49
    ),
    g2 = case_when(
      (type == "weak"                      ) ~ -.19,
      (type == "mixed" | type == "strong") ~ -.49
    ),
    g5 = case_when(
      (type == "weak" | type == "mixed") ~ -.19,
      (type == "strong"                  ) ~ -.49
    ),
    g6 = case_when(
      (type == "weak"                      ) ~ -.19,
      (type == "mixed" | type == "strong") ~ -.49
    ),
    scenario = 63 + 1:n()
  ) %>%
  select(scenario, everything())

# saveRDS(table, "data/processed/analysisIds.rds")
readr::write_csv(table, "data/processed/analysisIdsInteractions.csv")


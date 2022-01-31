#!/usr/bin/env Rscript

library(tidyverse)
library(readr)

base <- "interaction"
type <- c(
  "weak", 
  "mixed", 
  "strong",
  "negative-weak", 
  "negative-mixed", 
  "negative-strong",
  "combined"
)
sampleSize <- 4250
auc <- .75
harm <- c("absent", "moderate-positive", "strong-positive", "negative")

table <- expand_grid(base, type, sampleSize, auc, harm) %>%
  mutate(
    b0 = -2.08,
    # b0 = case_when(
    #   type == "weak" ~ -3.05,
    #   type == "mixed" ~ -3.32,
    #   TRUE ~ -3.50
    # ),
    b1 = .49,
    b2 = .49,
    b3 = .49,
    b4 = .49,
    b5 = .49,
    b6 = .49,
    b7 = .49,
    b8 = .49,
    g0 = log(.5),
    g1 = case_when(
      (type == "weak" | type == "mixed"                   ) ~ -.19,
      (type == "strong" | type == "combined"              ) ~ -.49,
      (type == "negative-weak" | type == "negative-mixed" ) ~ .16,
      (type == "negative-strong"                          ) ~ .33
    ),
    g2 = case_when(
      (type == "weak"                                                           ) ~ -.19,
      (type == "mixed" | type == "strong"                                       ) ~ -.49,
      (type == "negative-weak"                                                  ) ~ .16,
      (type == "negative-mixed" | type == "negative-strong" | type == "combined") ~ .33
    ),
    g5 = case_when(
      (type == "weak" | type == "mixed"                   ) ~ -.19,
      (type == "strong" | type =="combined"               ) ~ -.49,
      (type == "negative-weak" | type == "negative-mixed" ) ~ .16,
      (type == "negative-strong"                          ) ~ .33
    ),
    g6 = case_when(
      (type == "weak"                                                           ) ~ -.19,
      (type == "mixed" | type == "strong"                                       ) ~ -.49,
      (type == "negative-weak"                                                  ) ~ .16,
      (type == "negative-mixed" | type == "negative-strong" | type == "combined") ~ .33
    ),
    averageTrueBenefit = case_when(
      type == "weak" ~ .096,
      type == "mixed" ~ .104,
      type == "strong" ~ .111,
      type == "negative-weak" ~ .061,
      type == "negative-mixed" ~ .05,
      type == "negative-strong" ~ .039,
      type == "combined" ~ .076
    ),
    averageObservedBenefit = case_when(
      harm == "absent" ~ averageTrueBenefit,
      harm == "moderate-positive" & base == "absent" ~ averageTrueBenefit - .01,
      harm == "strong-positive" & base == "absent" ~ averageTrueBenefit - .02,
      harm == "negative" & base == "absent" ~ averageTrueBenefit + .01,
      harm == "moderate-positive" ~ 3 / 4 * averageTrueBenefit,
      harm == "strong-positive" ~ averageTrueBenefit / 2,
      harm == "negative" ~ 5 / 4 * averageTrueBenefit
    ),
    scenario = 648 + 1:n()
  ) %>%
  select(scenario, everything())

# saveRDS(table, "data/processed/analysisIds.rds")
readr::write_csv(table, "data/processed/analysisIdsInteractions.csv")

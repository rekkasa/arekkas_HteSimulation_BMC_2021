#!/usr/bin/env Rscript

# Description: Generates the analysisIds
# Output: data/processed/analysisIds.csv
# Depends: 

library(tidyverse)
library(readr)

base <- c(
  "absent",
  "moderate",
  "high"
)

type <- c(
  "constant",
  "linear-moderate",
  "linear-high",
  "quadratic-moderate",
  "quadratic-high",
  "non-monotonic"
)

auc <- c(.75, .65, .85)

sampleSize <- c(4250, 1063, 17000)
harm <- c("absent", "positive", "negative")

analysisIds <- expand_grid(base, type, sampleSize, auc, harm) %>%
  mutate(
    b0 = case_when(
      auc == .75 ~ -2.08,
      auc == .65 ~ -1.63,
      auc == .85 ~ -2.7,
      TRUE ~ 0
    ),
    b1 = case_when(
      auc == .75 ~ .49,
      auc == .65 ~ .26,
      auc == .85 ~ .82,
      TRUE ~ 0
    ),
    b2 = b1,
    b3 = b1,
    b4 = b1,
    b5 = b1,
    b6 = b1,
    b7 = b1,
    b8 = b1,
    g0 = case_when(
      base == "absent" & type == "constant" ~ 0,
      base == "moderate" & type == "constant" ~ log(.8),
      base == "high" & type == "constant" ~ log(.5),
      base == "absent" & type == "linear-moderate" & auc == .75 ~ -.06,
      base == "absent" & type == "linear-moderate" & auc == .65 ~ -.08,
      base == "absent" & type == "linear-moderate" & auc == .85 ~ -.07,
      base == "absent" & type == "linear-high" & auc == .75 ~ -.25,
      base == "absent" & type == "linear-high" & auc == .65 ~ -.29,
      base == "absent" & type == "linear-high" & auc == .85 ~ -.22,
      base == "absent" & type == "quadratic-moderate" & auc == .75 ~ -4.81,
      base == "absent" & type == "quadratic-moderate" & auc == .65 ~ -4.77,
      base == "absent" & type == "quadratic-moderate" & auc == .85 ~ -4.71,
      base == "absent" & type == "quadratic-high" & auc == .75 ~ -4.22,
      base == "absent" & type == "quadratic-high" & auc == .65 ~ -4.16,
      base == "absent" & type == "quadratic-high" & auc == .85 ~ -3.95,
      base == "absent" & type == "non-monotonic" & auc == .75 ~ 2.49,
      base == "absent" & type == "non-monotonic" & auc == .65 ~ 2.15,
      base == "absent" & type == "non-monotonic" & auc == .85 ~ 1.74,
      base == "moderate" & type == "linear-moderate" & auc == .75 ~ -.29,
      base == "moderate" & type == "linear-moderate" & auc == .65 ~ -.3,
      base == "moderate" & type == "linear-moderate" & auc == .85 ~ -.28,
      base == "moderate" & type == "linear-high" & auc == .75 ~ -.46,
      base == "moderate" & type == "linear-high" & auc == .65 ~ -.5,
      base == "moderate" & type == "linear-high" & auc == .85 ~ -.41,
      base == "moderate" & type == "quadratic-moderate" & auc == .75 ~ -5.02,
      base == "moderate" & type == "quadratic-moderate" & auc == .65 ~ -4.99,
      base == "moderate" & type == "quadratic-moderate" & auc == .85 ~ -4.91,
      base == "moderate" & type == "quadratic-high" & auc == .75 ~ -4.42,
      base == "moderate" & type == "quadratic-high" & auc == .65 ~ -4.38,
      base == "moderate" & type == "quadratic-high" & auc == .85 ~ -4.13,
      base == "moderate" & type == "non-monotonic" & auc == .75 ~ 0.1734843,
      base == "moderate" & type == "non-monotonic" & auc == .65 ~ 0.4805806,
      base == "moderate" & type == "non-monotonic" & auc == .85 ~ -0.08537934,
      base == "high" & type == "linear-moderate" & auc == .75 ~ -.75,
      base == "high" & type == "linear-moderate" & auc == .65 ~ -.77,
      base == "high" & type == "linear-moderate" & auc == .85 ~ -.73,
      base == "high" & type == "linear-high" & auc == .75 ~ -.9,
      base == "high" & type == "linear-high" & auc == .65 ~ -.96,
      base == "high" & type == "linear-high" & auc == .85 ~ -.84,
      base == "high" & type == "quadratic-moderate" & auc == .75 ~ -5.48,
      base == "high" & type == "quadratic-moderate" & auc == .65 ~ -5.46,
      base == "high" & type == "quadratic-moderate" & auc == .85 ~ -5.35,
      base == "high" & type == "quadratic-high" & auc == .75 ~ -4.86,
      base == "high" & type == "quadratic-high" & auc == .65 ~ -4.84,
      base == "high" & type == "quadratic-high" & auc == .85 ~ -4.51,
      base == "high" & type == "non-monotonic" & auc == .75 ~ -0.08413846,
      base == "high" & type == "non-monotonic" & auc == .65 ~ 0.7862489,
      base == "high" & type == "non-monotonic" & auc == .85 ~ -0.6206897,
      TRUE ~ 0
    ),
    g1 = case_when(
      type == "linear-moderate" & auc == .75 ~ 0.9472527,
      type == "linear-moderate" & auc == .65 ~ 0.9340659,
      type == "linear-moderate" & auc == .85 ~ 0.9296703,
      type == "linear-high" & auc == .75 ~ 0.7956044,
      type == "linear-high" & auc == .65 ~ 0.7758242,
      type == "linear-high" & auc == .85 ~ 0.7846154,
      base == "absent" & type == "non-monotonic" & auc == .75 ~ 4.21,
      base == "absent" & type == "non-monotonic" & auc == .65 ~ 3.32,
      base == "absent" & type == "non-monotonic" & auc == .85 ~ 4.1,
      base == "moderate" & type == "non-monotonic" & auc == .75 ~ 1.560407,
      base == "moderate" & type == "non-monotonic" & auc == .65 ~ 1.78266,
      base == "moderate" & type == "non-monotonic" & auc == .85 ~ 1.354139,
      base == "high" & type == "non-monotonic" & auc == .75 ~ 2.034903,
      base == "high" & type == "non-monotonic" & auc == .65 ~ 2.761839,
      base == "high" & type == "non-monotonic" & auc == .85 ~ 1.565517,
      TRUE ~ 1
    ),
    g2 = case_when(
      type == "quadratic-moderate" & auc == .75 ~ -0.01255887,
      type == "quadratic-moderate" & auc == .65 ~ -0.01642314,
      type == "quadratic-moderate" & auc == .85 ~ -0.01642314,
      type == "quadratic-high" & auc == .75 ~ -0.05168458,
      type == "quadratic-high" & auc == .65 ~ -0.05941311,
      type == "quadratic-high" & auc == .85 ~ -0.05941311,
      base == "absent" & type == "non-monotonic" & auc == .75 ~ .54,
      base == "absent" & type == "non-monotonic" & auc == .65 ~ .38,
      base == "absent" & type == "non-monotonic" & auc == .85 ~ .55,
      base == "moderate" & type == "non-monotonic" & auc == .75 ~ 0.105142,
      base == "moderate" & type == "non-monotonic" & auc == .65 ~ 0.1373087,
      base == "moderate" & type == "non-monotonic" & auc == .85 ~ 0.0742429,
      base == "high" & type == "non-monotonic" & auc == .75 ~ 0.2103462,
      base == "high" & type == "non-monotonic" & auc == .65 ~ 0.3209179,
      base == "high" & type == "non-monotonic" & auc == .85 ~ 0.137931,
      TRUE ~ 0
    ),
    c = case_when(
      startsWith(type, "quad") ~ -5,
      TRUE ~ 0
    ),
    scenario = 1:486,
    averageTrueBenefit = case_when(
      base == "absent" ~ 0,
      base == "moderate" & auc == .75 ~ .029,
      base == "moderate" & auc == .65 ~ .033,
      base == "moderate" & auc == .85 ~ .024,
      base == "high" & auc == .75 ~ .079,
      base == "high" & auc == .65 ~ .089,
      base == "high" & auc == .85 ~ .069,
    )
  ) %>%
  relocate(scenario) %>%
  write_csv(path = "data/processed/analysisIds.csv")
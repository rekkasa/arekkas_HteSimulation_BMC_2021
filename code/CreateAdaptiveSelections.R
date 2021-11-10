#!/usr/bin/env Rscript

# --------------------------------------------------
# Description: 
#   Creates a csv file with the models selected
#   in each run of the simulation
# Output: 
#   - data/processed/adaptiveSelections.csv
# Depends:
#   - data/processed/adaptiveModel.csv
# --------------------------------------------------

library(tidyverse)

readr::read_csv("data/processed/adaptiveModel.csv") %>%
  group_by(scenarioId) %>% 
  nest() %>% 
  mutate(
    counts = lapply(data, plyr::count),
    n = lapply(data, nrow)
  ) %>%
  select(
    scenarioId, counts, n
  ) %>%
  unnest(c(counts, n)) %>%
  mutate(
    perc = round(freq / n, 2),
    model = case_when(
      selectedAdaptiveModel == "RCS with 3 knots" ~ "rcs_3",
      selectedAdaptiveModel == "RCS with 4 knots" ~ "rcs_4",
      selectedAdaptiveModel == "RCS with 5 knots" ~ "rcs_5",
      TRUE                                        ~ selectedAdaptiveModel
    )
  ) %>%
  ungroup() %>%
  select(scenarioId, model, perc) %>%
  spread(model, perc) %>%
  relocate(c(treatment, risk), .before = rcs_3) %>%
  readr::write_csv("data/processed/adaptiveSelections.csv")

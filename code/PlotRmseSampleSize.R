#!/usr/bin/env Rscript

# Description: 
# Output: 
# Depends: 

library(tidyverse)
library(glue)
source("code/helpers/CreateManuscriptPlots.R")
source("code/helpers/PlotResult.R")

scenarioIds <- readr::read_csv("data/processed/analysisIds.csv") %>%
  filter(
    base == "moderate",
    !(type %in% c("moderate-linear", "moderate-quadratic")),
    sampleSize == 17000,
    auc == .75
  )
metric    <- "rmse"
value     <- "sample_size"

titles <- scenarioIds %>%
  mutate(
    title = ifelse(
      type == "constant",
      str_to_sentence(glue("{str_replace_all(type, '-', ' ')} treatment effect")),
      str_to_sentence(glue("{str_replace_all(type, '-', ' ')} deviation"))
    )
  ) %>%
  select(title) %>%
  unlist() %>%
  unique()

names(titles) <- NULL

titlePrefix <- paste0(
  "**",
  LETTERS[1:4],
  ".**"
)

titles <- paste(
  titlePrefix,
  titles
)

metricFile <- paste(metric, "csv", sep = ".")

f <- function(x) x * 100

processed <- readr::read_csv(
  file = file.path("data/processed", metricFile)
) %>% 
  select(-one_of("locfit")) %>%
  mutate_at(
    c(
      "constant_treatment_effect",
      "stratified",
      "linear_predictor",
      "rcs_3_knots",
      "rcs_4_knots",
      "rcs_5_knots",
      "adaptive"
    ),
    f
  )

scenarios <- scenarioIds %>%
  filter(harm == "absent") %>%
  select(scenario) %>%
  unlist()
names(scenarios) <- NULL

plotList <- plotResult(scenarios, processed, titles, metric = metric)

pp <- gridExtra::grid.arrange(
  plotList[[1]] + theme(axis.text.x = element_blank()),
  plotList[[2]] + theme(legend.position = "none", axis.text.x = element_blank()),
  plotList[[3]] + theme(legend.position = "none"),
  plotList[[4]] + theme(legend.position = "none"),
  heights = c(1, 1.3),
  nrow = 2,
  ncol = 2,
  left = grid::textGrob(
    expression(
      paste(
        "Root mean squared error (x",
        10^-2,
        ")"
      )
    ),
    rot = 90
  )
)

fileName <- paste0(
  paste(
    metric,
    value,
    sep = "_"
  ),
  ".tiff"
)

ggplot2::ggsave(
  file.path("figures", fileName), 
  plot = pp,
  dpi = 1200,
  width = 10, 
  height = 7,
  compression = "lzw"
)
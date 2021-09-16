#!/usr/bin/env Rscript

# Description: 
# Output: 
# Depends: 

library(tidyverse)
source("code/helpers/CreateManuscriptPlots.R")
source("code/helpers/PlotResult.R")

scenarioIds <- readr::read_csv("data/processed/analysisIds.csv")

scenarios <- c(
  5, 4, 6,
  2, 1, 3,
  8, 7, 9
)
metric    <- "rmse"
value     <- "absent"

titles <- scenarioIds %>%
  filter(scenario %in% scenarios) %>%
  select(title) %>%
  unlist()

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

plotList <- plotResult(scenarios, processed, titles = NULL)

pp <- gridExtra::grid.arrange(
  plotList[[1]],
  plotList[[2]],
  plotList[[3]],
  plotList[[4]],
  plotList[[5]],
  plotList[[6]],
  plotList[[7]],
  plotList[[8]],
  plotList[[9]],
  nrow = 3,
  ncol = 3,
  left = grid::textGrob(
    "Sample size",
    rot = 90
  ),
  top = grid::textGrob(
    "Prediction AUC"
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
    width = 10.5, 
    height = 8,
    compression = "lzw"
  )
  
  
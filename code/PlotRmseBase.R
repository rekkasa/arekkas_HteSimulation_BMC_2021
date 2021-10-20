#!/usr/bin/env Rscript

# Description: 
# Output: 
# Depends: 

library(tidyverse)
source("code/helpers/CreateManuscriptPlots.R")
source("code/helpers/PlotResult.R")

scenarioIds <- readr::read_csv("data/processed/analysisIds.csv")

scenarios <- c(10, 28, 37, 55)
metric    <- "rmse"
value     <- "base"

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

plotList <- plotResult(scenarios, processed, titles)

pp <- gridExtra::grid.arrange(
  plotList[[1]],
  plotList[[2]],
  plotList[[3]],
  plotList[[4]],
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
    width = 8.5, 
    height = 7,
    compression = "lzw"
  )
  

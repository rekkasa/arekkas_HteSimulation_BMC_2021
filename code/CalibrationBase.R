#!/usr/bin/env Rscript

# ===================================================
# Description:
#   Generates the calibration plots
# Input:
#   - sample size
#   - auc
#   - value
# Output:
#   - figures/calibration_*.tiff
# Depends:
#   - data/processed/analysisIds.csv
#   - data/processed/calibration.csv
#   - code/helpers/CreateManuscriptPlots.R
#   - code/helpers/PlotResult.R
# ===================================================

args <- commandArgs(trailingOnly = TRUE)
args_base <- as.character(args[1])
args_sampleSize <- as.numeric(args[2])
args_auc <- as.numeric(args[3])
args_value <- as.character(args[4])

library(tidyverse)
library(glue)
library(ggtext)
source("code/helpers/CreateManuscriptPlots.R")
source("code/helpers/PlotResult.R")

scenarioIds <- readr::read_csv("data/processed/analysisIds.csv") %>%
  filter(
    base == args_base,
    harm != "negative",
    !(type %in% c("quadratic-moderate", "linear-moderate")),
    sampleSize == args_sampleSize,
    auc == args_auc
  ) 

metric    <- "calibration"

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

plotList <- plotResult(scenarios, processed, titles, metric = metric, limits = c(0, 6, 1))

res <- gridExtra::grid.arrange(
  plotList[[1]] +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_markdown(),
      axis.title = element_blank(),
      legend.direction = "horizontal",
      legend.position = c(.432, .93),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      axis.text.x = element_blank()
    ),
  plotList[[2]] +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_markdown(),
      axis.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_blank()
    ),
  plotList[[3]] +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_markdown(),
      axis.title = element_blank(),
      legend.position = "none"
    ),
  plotList[[4]] +
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      plot.title = element_markdown(),
      legend.position = "none"
    ),
  heights = c(1, 1.05),
  nrow = 2,
  ncol = 2,
  left = grid::textGrob(
    expression(
      paste(
        "Calibration for benefit (x",
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
    args_base,
    args_value,
    sep = "_"
  ),
  ".tiff"
)
  ggplot2::ggsave(
    file.path("figures", fileName), 
    plot = res,
    dpi = 1200,
    width = 10, 
    height = 7,
    compression = "lzw"
  )
  
  # ggplot2::ggsave(
  #   file.path("figures", fileName), 
  #   plot = pp,
  #   width = 8.5, 
  #   height = 7
  # )
  

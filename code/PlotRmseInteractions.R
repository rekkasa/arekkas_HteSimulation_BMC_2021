#!/usr/bin/env Rscript

# ===================================================
# Description:
#   Generates the RMSE plots for the base case
# Input:
#   - type
#   - value
# Output:
#   - figures/rmse_base.tiff
# Depends:
#   - data/processed/analysisIds.csv
#   - data/processed/rmse.csv
#   - code/helpers/CreateManuscriptPlots.R
#   - code/helpers/PlotResult.R
#   - code/helpers/Absolute.R
# ===================================================

library(tidyverse)
library(glue)
library(ggtext)
library(gridExtra)
library(grid)
library(ggside)

args <- commandArgs(trailingOnly = TRUE)
interactionType <- as.character(args[1])

if (interactionType == "positive") {
  scenarioLimits <- c(649, 660)
} else if (interactionType == "negative") {
  scenarioLimits <- c(661, 672)
} else {
  scenarioLimits <- c(673, 676)
}


source("code/helpers/CreateManuscriptPlots.R")     # Geneartes single boxplot
source("code/helpers/PlotResult.R")                # Generates the boxplot list
source("code/helpers/Absolute.R")                  # Generates the absolute plots

scenarioIds <- readr::read_csv("data/processed/analysisIdsInteractions.csv") %>%
  filter(scenario >= scenarioLimits[1] & scenario <= scenarioLimits[2])

metric    <- "rmse"
titles <- scenarioIds %>%
  mutate(
    title = str_to_sentence(glue("{str_replace_all(type, '-', ' ')} interactions")
    )
  ) %>%
  select(title) %>%
  unlist() %>%
  unique()

titlePrefix <- paste0(
  "**",
  LETTERS[1:3],
  ".**"
)

titles <- paste(
  titlePrefix,
  titles
)

metricFile <- paste(metric, "csv", sep = ".")

f <- function(x) x * 100

limitsHigh <- .15

processed <- readr::read_csv(
  file = file.path("data/processed", metricFile)
) %>% 
  filter(scenarioId >= scenarioLimits[1] & scenarioId <= scenarioLimits[2]) %>%
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

plotList <- plotResult(scenarios, processed, titles, metric = metric, limits = c(0, 10, 2.5))

if (interactionType == "combined") {
  res <- plotList[[1]] +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_markdown(size = 9),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      legend.title = element_text(size = 7.5),
      legend.text = element_text(size = 7)
    )
} else {
  gridList <- list(
    plotList[[1]] +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(size = 9),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8),
        legend.direction = "horizontal",
        legend.title = element_text(size = 7.5),
        legend.text = element_text(size = 7),
        legend.position = c(.211, .910)
      ),
    plotList[[2]] + 
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(size = 9),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8),
        ggside.line = element_blank(),
        ggside.rect = element_blank(),
        ggside.axis.text = element_blank(),
        ggside.axis.ticks.length = unit(0, "pt"),
        ggside.panel.scale = .07,
        legend.position = "none"
      ),
    plotList[[3]] +
      theme(
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "none",
        plot.title = element_markdown(size = 9)
      )
  )
  
  plot <- cowplot::plot_grid(
    gridList[[1]],
    gridList[[2]],
    gridList[[3]],
    ncol = 1
  )
  
  left.grob <- grid::textGrob(
      expression(
        paste(
          "Root mean squared error (x",
          10^-2,
          ")"
        )
      ),
      rot = 90
  )
  
  res <- grid.arrange(arrangeGrob(plot, left = left.grob))
}


fileName <- paste0(
  paste(
    metric,
    "interaction",
    interactionType,
    sep = "_"
  ),
  ".tiff"
)
  ggplot2::ggsave(
    file.path("figures", fileName), 
    plot = res,
    dpi = 1200,
    width = 10, 
    height = 8,
    compression = "lzw"
  )
  

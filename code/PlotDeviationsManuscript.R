#!/usr/bin/env Rscript

# Description: 
# Output: 
# Depends: 

source("code/helpers/PlotGammas.R")

library(tidyverse)
library(SmoothHte)
library(SimulateHte)
library(SimulationEvaluationHte)
library(dplyr)

analysisIds <- readr::read_csv(
  "data/processed/analysisIds.csv",
  col_types = "iffiiddddddddddddd"
)

analysisIdsInteractions <- readr::read_csv(
  "data/processed/analysisIdsInteractions.csv",
  col_types = "icfiidddddddddddddd"
)

generatePlot <- function(scenarios) {
  
  gammas <- list()
  for (i in seq_along(scenarios)) {
    idSettings <- analysisIds %>%
      filter(scenario == scenarios[i])
    gammas[[i]] <- list(
      g0 = idSettings$g0,
      g1 = idSettings$g1,
      g2 = idSettings$g2,
      c  = idSettings$c
    )
  }
  
  names(gammas) <- c(
    "gammas_base_case",
    "gammas_linear_moderate",
    "gammas_linear_high",
    "gammas_quadratic_high"
  )
  
  p <- constantEffectPlot(or = .8, label = "base-case") +
    plotSquare(g0 = 0, g1 = 1, label = "no effect", linetype = "dashed") +
    plotSquare(
      g0    = gammas$gammas_linear_moderate$g0,
      g1    = gammas$gammas_linear_moderate$g1,
      g2    = gammas$gammas_linear_moderate$g2,
      l     = gammas$gammas_linear_moderate$c,
      label = "linear-moderate"
    ) +
    plotSquare(
      g0    = gammas$gammas_linear_high$g0,
      g1    = gammas$gammas_linear_high$g1,
      g2    = gammas$gammas_linear_high$g2,
      l     = gammas$gammas_linear_high$c,
      label = "linear-strong"
    ) +
    plotSquare(
      g0    = gammas$gammas_quadratic_high$g0,
      g1    = gammas$gammas_quadratic_high$g1,
      g2    = gammas$gammas_quadratic_high$g2,
      l     = gammas$gammas_quadratic_high$c,
      label = "quadratic-strong"
    ) +
    xlab("Linear predictor in control arm") +
    ylab("Linear predictor in treatment arm") +
    xlim(-5, 2) +
    scale_color_manual(
      values = c("black", "#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"),
      breaks = c("no effect", "base-case", "linear-moderate", "linear-strong", "quadratic-strong")
    ) +
    theme_classic() +
    theme(
      axis.text       = element_text(size = 10),
      axis.title      = element_text(size = 11),
      legend.text     = element_text(size = 11),
      legend.title    = element_blank(),
      legend.position = c(.8, .2)
    )
  
  return(p)
}


generateAbsolutePlot <- function(scenarios) {
  gammas <- list()
  for (i in seq_along(scenarios)) {
    idSettings <- analysisIds %>%
      filter(scenario == scenarios[i])
    gammas[[i]] <- list(
      g0 = idSettings$g0,
      g1 = idSettings$g1,
      g2 = idSettings$g2,
      c  = idSettings$c
    )
  }
  
  names(gammas) <- c(
    "gammas_base_case",
    "gammas_linear_moderate",
    "gammas_linear_high",
    "gammas_quadratic_high"
  )
  
  ggplot() +
    plotAbsoluteBenefit(
      log(.8), 
      g1 = 1, 
      l = 0, label = "base-case"
      ) +
    plotAbsoluteBenefit(
      g0    = gammas$gammas_linear_moderate$g0,
      g1    = gammas$gammas_linear_moderate$g1,
      g2    = gammas$gammas_linear_moderate$g2,
      l     = gammas$gammas_linear_moderate$c,
      label = "base-case"
    ) +
    plotAbsoluteBenefit(
      g0    = gammas$gammas_linear_moderate$g0,
      g1    = gammas$gammas_linear_moderate$g1,
      g2    = gammas$gammas_linear_moderate$g2,
      l     = gammas$gammas_linear_moderate$c,
      label = "linear-moderate"
    ) +
    plotAbsoluteBenefit(
      g0    = gammas$gammas_linear_high$g0,
      g1    = gammas$gammas_linear_high$g1,
      g2    = gammas$gammas_linear_high$g2,
      l     = gammas$gammas_linear_high$c,
      label = "linear-strong"
    ) +
    plotAbsoluteBenefit(
      g0    = gammas$gammas_quadratic_high$g0,
      g1    = gammas$gammas_quadratic_high$g1,
      g2    = gammas$gammas_quadratic_high$g2,
      l     = gammas$gammas_quadratic_high$c,
      label = "quadratic-strong"
    ) +
    xlim(0, .5) +
    xlab("Baseline risk") +
    ylab("Absolute benefit") +
    theme_classic() +
    scale_color_manual(
      values = c("black", "#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"),
      breaks = c("no effect", "base-case", "linear-moderate", "linear-strong", "quadratic-strong")
    ) +
    theme(
      axis.text       = element_text(size = 10),
      axis.title      = element_text(size = 11),
      legend.position = "none"
    )
}


scenarios               <- c(10, 28, 37, 55)
plotsManuscript         <- generatePlot(scenarios = scenarios)
plotsAbsoluteManuscript <- generateAbsolutePlot(scenarios)

ggsave(
  filename    = "figures/deviationsManuscript.png", 
  plot        = plotsManuscript,
  width       = 12,
  height      = 12,
  units       = "cm"
)
ggsave(
  filename    = "figures/deviationsManuscript.tiff", 
  plot        = plotsManuscript,
  dpi         = 1200,
  device      = "tiff",
  compression = "lzw"
)
ggsave(
  filename    = "figures/deviationsAbsoluteManuscript.png", 
  plot        = plotsAbsoluteManuscript,
  width       = 12,
  height      = 12,
  units       = "cm"
)
ggsave(
  filename    = "figures/deviationsAbsoluteManuscript.tiff", 
  plot        = plotsAbsoluteManuscript,
  dpi         = 1200,
  device      = "tiff",
  compression = "lzw"
)

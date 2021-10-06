#!/usr/bin/env Rscript

# Description:
#   Plots the theoretical deviations from the base case scenario
# Output:
#   figures/deviate_linear_08.png
#   figures/deviate_quadratic_08.png
#   figures/deviate_linear_absolute_08.png
#   figures/deviate_quadratic_absolute_08.png
# Depends:
#   code/PlotGammas.R



source("code/helpers/PlotGammas.R")

library(tidyverse)
library(SmoothHte)
library(SimulateHte)
library(SimulationEvaluationHte)
library(dplyr)

analysisIds <- readr::read_csv(
  "data/processed/analysisIds.csv",
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
    "gammas_moderate",
    "gammas_high"
  )
  
  p <- constantEffectPlot(or = .8, label = "base-case") +
    plotSquare(g0 = 0, g1 = 1, label = "no effect", linetype = "dashed") +
    plotSquare(
      g0    = gammas$gammas_moderate$g0,
      g1    = gammas$gammas_moderate$g1,
      g2    = gammas$gammas_moderate$g2,
      l     = gammas$gammas_moderate$c,
      label = "moderate"
    ) +
    plotSquare(
      g0    = gammas$gammas_high$g0,
      g1    = gammas$gammas_high$g1,
      g2    = gammas$gammas_high$g2,
      l     = gammas$gammas_high$c,
      label = "high"
    ) +
    xlab("Linear predictor in control arm") +
    ylab("Linear predictor in treatment arm") +
    xlim(-5, 2) +
    scale_color_manual(
      values = c("black", "#66c2a5", "#fc8d62", "#8da0cb"),
      breaks = c("no effect", "base-case", "moderate", "high")
    ) +
    theme_classic() +
    theme(
      axis.text    = element_text(size = 19),
      axis.title   = element_text(size = 21),
      legend.text  = element_text(size = 15),
      legend.title = element_blank()
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
    "gammas_moderate",
    "gammas_high"
  )
  
  ggplot() +
    plotAbsoluteBenefit(
      log(.8), 
      g1 = 1, 
      l = 0, label = "base-case"
      ) +
    plotAbsoluteBenefit(
      g0    = gammas$gammas_moderate$g0,
      g1    = gammas$gammas_moderate$g1,
      g2    = gammas$gammas_moderate$g2,
      l     = gammas$gammas_moderate$c,
      label = "moderate"
    ) +
    plotAbsoluteBenefit(
      g0    = gammas$gammas_high$g0,
      g1    = gammas$gammas_high$g1,
      g2    = gammas$gammas_high$g2,
      l     = gammas$gammas_high$c,
      label = "high"
    )  +
    xlim(0, .5) +
    xlab("Baseline risk") +
    ylab("Absolute benefit") +
    theme_classic() +
    scale_color_manual(
      values = c("#66c2a5", "#fc8d62", "#8da0cb"),
      breaks = c("base-case", "moderate", "high")
    ) +
    theme(
      axis.text = element_text(size = 23),
      axis.title = element_text(size = 25),
      legend.text = element_text(size = 20),
      legend.title = element_blank()
    ) 
}


scenariosLinear       <- c(28, 37)
scenariosQudratic     <- c(46, 55)
plotLinear            <- generatePlot(scenariosLinear)
plotLinearAbsolute    <- generateAbsolutePlot(scenariosLinear)
plotQuadratic         <- generatePlot(scenariosQudratic)
plotQuadraticAbsolute <- generateAbsolutePlot(scenariosQudratic)

ggsave(filename = "figures/deviate_linear_08.png",             plot = plotLinear           )
ggsave(filename = "figures/deviate_quadratic_08.png",          plot = plotQuadratic        )
ggsave(filename = "figures/deviate_linear_absolute_08.png",    plot = plotLinearAbsolute   )
ggsave(filename = "figures/deviate_quadratic_absolute_08.png", plot = plotQuadraticAbsolute)

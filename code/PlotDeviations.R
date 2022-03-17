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
  "data/processed/analysisIds.csv"
  )

analysisIdsInteractions <- readr::read_csv(
  "data/processed/analysisIdsInteractions.csv",
  col_types = "icfiidddddddddddddd"
)

generatePlot <- function(scenarios, analysisIds) {
  
  gammas <- deviationPlots <- list()
  idSettings <- analysisIds %>% filter(scenario %in% scenarios)
  for (i in seq_along(scenarios)) {
    gammas[[i]] <- list(
      g0 = idSettings %>% filter(scenario == scenarios[i]) %>% pull(g0),
      g1 = idSettings %>% filter(scenario == scenarios[i]) %>% pull(g1),
      g2 = idSettings %>% filter(scenario == scenarios[i]) %>% pull(g2),
      c  = idSettings %>% filter(scenario == scenarios[i]) %>% pull(c)
    )
  }

  names(gammas) <- sub("^[lq].*-", "", idSettings$type)
  
  initialPlot <- constantEffectPlot(or = .8, label = "base-case") +
    plotSquare(g0 = 0, g1 = 1, label = "no effect", linetype = "dashed")
  types <- names(gammas)


  deviationPlots <- map(
    types,
    function(type) {
      plotSquare(
        g0 = gammas[[type]]$g0,
        g1 = gammas[[type]]$g1,
        g2 = gammas[[type]]$g2,
        l = gammas[[type]]$c,
        label = type
      )
    }
  )
  p <- initialPlot +
    deviationPlots[[1]]
  if (length(types) > 1) {
    p <- p + deviationPlots[[2]]
  }
  p <- p +
    xlab("Linear predictor in control arm") +
    ylab("Linear predictor in treatment arm") +
    xlim(-5, 2) +
    scale_color_manual(
      values = c("black", "#66c2a5", "#fc8d62", "#8da0cb", "red"),
      breaks = c("no effect", "base-case", "moderate", "high", "non-monotonic")
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


generateAbsolutePlot <- function(scenarios, analysisIds) {
  gammas <- deviationPlots <- list()
  idSettings <- analysisIds %>% filter(scenario %in% scenarios)
  for (i in seq_along(scenarios)) {
    gammas[[i]] <- list(
      g0 = idSettings %>% filter(scenario == scenarios[i]) %>% pull(g0),
      g1 = idSettings %>% filter(scenario == scenarios[i]) %>% pull(g1),
      g2 = idSettings %>% filter(scenario == scenarios[i]) %>% pull(g2),
      c  = idSettings %>% filter(scenario == scenarios[i]) %>% pull(c)
    )
  }

  names(gammas) <- sub("^[lq].*-", "", idSettings$type)
  
  initialPlot <- ggplot() +
    plotAbsoluteBenefit(
      log(.8), 
      g1 = 1, 
      l = 0, label = "base-case"
    )

  types <- names(gammas)

  deviationPlots <- map(
    types,
    function(type) {
      plotAbsoluteBenefit(
        g0 = gammas[[type]]$g0,
        g1 = gammas[[type]]$g1,
        g2 = gammas[[type]]$g2,
        l = gammas[[type]]$c,
        label = type
      )
    }
  )

  p <- initialPlot +
    deviationPlots[[1]]
  if (length(types) > 1) {
    p <- p + deviationPlots[[2]]
  }
  
  p <- p +
    xlim(0, .5) +
    xlab("Baseline risk") +
    ylab("Absolute benefit") +
    theme_classic() +
    scale_color_manual(
      values = c("#66c2a5", "#fc8d62", "#8da0cb", "red"),
      breaks = c("base-case", "moderate", "high", "non-monotonic")
    ) +
    theme(
      axis.text = element_text(size = 23),
      axis.title = element_text(size = 25),
      legend.text = element_text(size = 20),
      legend.title = element_blank()
    ) 
  return(p)
}


scenariosLinear       <- c(253, 289)
scenariosQudratic     <- c(325, 361)
scenariosNonMonotonic <- 397

plotLinear               <- generatePlot(scenariosLinear, analysisIds)
plotLinearAbsolute       <- generateAbsolutePlot(scenariosLinear, analysisIds)
plotQuadratic            <- generatePlot(scenariosQudratic, analysisIds)
plotQuadraticAbsolute    <- generateAbsolutePlot(scenariosQudratic, analysisIds)
plotNonMonotonic         <- generatePlot(scenariosNonMonotonic, analysisIds)
plotNonMonotonicAbsolute <- generateAbsolutePlot(scenariosNonMonotonic, analysisIds)

ggsave(filename = "figures/deviate_linear_08.png",                 plot = plotLinear)
ggsave(filename = "figures/deviate_quadratic_08.png",              plot = plotQuadratic)
ggsave(filename = "figures/deviate_non_monotonic_08.png",          plot = plotNonMonotonic)
ggsave(filename = "figures/deviate_linear_absolute_08.png",        plot = plotLinearAbsolute)
ggsave(filename = "figures/deviate_quadratic_absolute_08.png",     plot = plotQuadraticAbsolute)
ggsave(filename = "figures/deviate_non_monotonic_absolute_08.png", plot = plotNonMonotonicAbsolute)

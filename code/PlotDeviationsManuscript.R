#!/usr/bin/env Rscript

# Description: 
# Output: 
# Depends: 

source("code/helpers/PlotGammas.R")
source("code/helpers/PlotDeviationsFunctions.R")

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

#!/usr/bin/env Rscript

# Description:
#   Script to be run for each simulation scenario
# Depends:
#   data/processed/analysisIds.rds
# Output:
#   data/processed/scenario_#/
# Note:
#   Needs a data/raw/scenario_# as input

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop("Requires scenario id")

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

selectedScenario <- as.numeric(
  stringr::str_extract(args[1], "[0-9]+")
)

if (selectedScenario <= 63) {
  idSettings <- analysisIds %>%
    filter(scenario == selectedScenario)
  createF1 <- function(c) function(x) x - c
  createF2 <- function(c) function(x) (x - c)^2
  treatmentEffectSettings <- SimulateHte::createTreatmentEffectSettings(
    type = "lp",
    modelSettings = SimulateHte::createModelSettings(
      constant = idSettings$g0,
      modelMatrix = matrix(c(1, 1)),
      transformationSettings = list(
        createF1(idSettings$c),
        createF2(idSettings$c)
      ),
      coefficients = c(
        idSettings$g1,
        idSettings$g2
      )
    )
  )
} else {
  idSettings <- analysisIdsInteractions %>%
    filter(scenario == selectedScenario)
  treatmentEffectSettings <- SimulateHte::createTreatmentEffectSettings(
    type = "covariates",
    modelSettings = SimulateHte::createModelSettings(
      constant = idSettings$g0,
      modelMatrix = rbind(
        c(1, rep(0, 7)),
        c(0, 1, rep(0, 6)),
        c(rep(0, 4), 1, 0, 0, 0),
        c(rep(0, 5), 1, 0, 0)
      ),
      transformationSettings = list(
        identity,
        identity,
        identity,
        identity
      ),
      coefficients = idSettings %>% select(matches("g[1-9]")) %>% unlist()
    )
  )
}

databaseSettings <- SimulateHte::createDatabaseSettings(
  numberOfObservations = as.numeric(as.character(idSettings$sampleSize)),
  numberOfCovariates = 8,
  covariateDistributionSettings = list(
    SimulateHte::createNormalDistributionSettings(),
    SimulateHte::createNormalDistributionSettings(),
    SimulateHte::createNormalDistributionSettings(),
    SimulateHte::createNormalDistributionSettings(),
    SimulateHte::createBinomialDistributionSettings(prob = .2),
    SimulateHte::createBinomialDistributionSettings(prob = .2),
    SimulateHte::createBinomialDistributionSettings(prob = .2),
    SimulateHte::createBinomialDistributionSettings(prob = .2)
  )
)

baselineRiskSettings <- SimulateHte::createBaselineRiskSettings(
  type = "binary",
  modelSettings = SimulateHte::createModelSettings(
    constant = idSettings %>% pull(b0),
    modelMatrix = diag(8),
    transformationSettings = list(
      identity,
      identity,
      identity,
      identity,
      identity,
      identity,
      identity,
      identity
    ),
    coefficients = idSettings %>% select(paste0("b", 1:8)) %>% unlist()
  )
)


propensitySettings <- SimulateHte::createPropensitySettings(
  type = "binary",
  modelSettings = SimulateHte::createModelSettings(
    constant = 0,
    modelMatrix = diag(0),
    transformationSettings = NULL
  )
)

simulationSettings <- list(
  databaseSettings        = databaseSettings,
  propensitySettings      = propensitySettings,
  baselineRiskSettings    = baselineRiskSettings,
  treatmentEffectSettings = treatmentEffectSettings
)

analysisSettings <- SimulationEvaluationHte::createAnalysisSettings(
  threads        = 4,
  seed           = 19910930,
  replications   = 100,
  validationSize = 1e5,
  analysisId     = paste(
    "scenario",
    idSettings$scenario,
    sep = "_"
  ),
  description = "description",
  saveDirectory = "data/raw"
)

smoothSettings <- list(
  constant = createHteSettings(
    label = "constant_treatment_effect",
    settings = createModelBasedSettings(
      type = "treatment",
      model = "logistic"
    )
  ),
  stratified = createHteSettings(
    settings = SmoothHte::createStratifiedSettings(),
    label = "stratified"
  ),
  constantLp = createHteSettings(
    label = "linear_predictor",
    settings = createModelBasedSettings()
  ),
  loess = createHteSettings(
    settings = SmoothHte::createLoessSettings(),
    label = "loess"
  ),
  rcs3 = createHteSettings(
    settings = SmoothHte::createRcsSettings(),
    label = "rcs_3_knots"
  ),
  rcs4 = createHteSettings(
    settings = SmoothHte::createRcsSettings(nKnots = 4),
    label = "rcs_4_knots"
  ),
  rcs5 = createHteSettings(
    settings = SmoothHte::createRcsSettings(nKnots = 5),
    label = "rcs_5_knots"
  ),
  locfit = createHteSettings(
    settings = SmoothHte::createLocfitSettings(),
    label = "locfit"
  ),
  adaptive = createHteSettings(
    settings = createAdaptiveSettings(
      list(
        nonLinearSettings = SmoothHte::createRcsSettings(),
        linearSettings    = SmoothHte::createModelBasedSettings(),
        constantSettings  = SmoothHte::createModelBasedSettings(
          type                 = "treatment",
          model                = "logistic"
        )
      )
    ),
    label = "adaptive"
  )
)

predictionSettings <- createPredictionSettings(
  args = list(
    formula = "outcome ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + treatment",
    family = "binomial"
  ),
  fun = "glm"
)

res <- runAnalysis(
  analysisSettings   = analysisSettings,
  simulationSettings = simulationSettings,
  predictionSettings = predictionSettings,
  smoothSettings     = smoothSettings
)


#!/usr/bin/env Rscript

# Description:
#   Generates the tables of c and ICI for benefit of all the methods used in the
#   GUSTO-I application.
# Output: 
#   data/processed/gustoPerformanceMetrics.csv
# Depends:
#   data/raw/gusto.rda

library(tidyverse)
library(rms)
library(SmoothHte)

load("data/raw/gusto.rda")
set.seed(19910930)

gusto <- gusto %>%
  tibble() %>%
  rename(
    "outcome" = "day30",
    "treatment" = "tpa"
  )

replicationExpression <- function(gusto) {
  prediction <- rms::lrm(
    outcome ~ treatment + age + Killip + pmin(sysbp, 120) + lsp(pulse, 50) + pmi + miloc,
    data = gusto,
    maxit = 99
  )
  
  riskLinearPredictor <- predict(
    prediction,
    newdata = gusto %>% mutate(treatment = 0) %>% data.frame()
  )
  
  gusto <- gusto %>%
    mutate(
      riskLinearPredictor = riskLinearPredictor
    )
  
  folds <- caret::createFolds(gusto$outcome, k = 4)
  
  evaluatePerformance <- function(data, testRows, settings) {
    isStratified <- FALSE
    if ("modelBased" %in% class(settings)) {
      model <- fitModelBasedHte(data %>% dplyr::slice(-testRows), settings)
      tmp <- data %>%
        slice(testRows) %>%
        dplyr::mutate(
          predictedBenefit = predictBenefitModelBasedHte(
            p             = plogis(.$riskLinearPredictor), 
            modelBasedFit = model
          )
        )
      
    } else if ("rcs" %in% class(settings)) {
      model <- fitRcsHte(data %>% dplyr::slice(-testRows), settings)
      tmp <- gusto %>%
        slice(testRows) %>%
        dplyr::mutate(
          predictedBenefit = predictSmoothBenefit(
            p          = plogis(riskLinearPredictor), 
            smoothFit  = model
          )
        )
    } else if ("stratified" %in% class(settings)) {
      isStratified <- TRUE
      model <- fitStratifiedHte(data %>% dplyr::slice(-testRows), settings)
      tmp <- gusto %>%
        slice(testRows) %>%
        dplyr::mutate(
          predictedBenefit = predictStratifiedBenefit(
            p          = plogis(riskLinearPredictor), 
            stratifiedHte  = model
          )
        )
    }
    cForBenefit <- calculateCForBenefit(tmp)
    iciForBenefit <- calculateCalibrationForBenefit(tmp)
    aic <- ifelse(
      isStratified,
      NA,
      model$aic
    )
    
    return(
      list(
        c = cForBenefit,
        ici = iciForBenefit$ici,
        aic = aic
      )
    )
  }
  
  
  constant <- lapply(
    folds,
    evaluatePerformance,
    data = gusto,
    settings = createModelBasedSettings(type = "treatment")
  )
  
  constantData <- do.call(bind_rows, constant)
  cConstant <- mean(constantData$c)
  iciConstant <- mean(constantData$ici)
  aicConstnat <- mean(constantData$aic)
  
  stratified <- lapply(
    folds,
    evaluatePerformance,
    data = gusto,
    settings = createStratifiedSettings()
  )
  
  stratifiedData <- do.call(bind_rows, stratified)
  cStratified <- mean(stratifiedData$c)
  iciStratified <- mean(stratifiedData$ici)
  
  
  linear <- lapply(
    folds,
    evaluatePerformance,
    data = gusto,
    settings = createModelBasedSettings()
  )
  
  linearData <- do.call(bind_rows, linear)
  clinear <- mean(linearData$c)
  iciLinear <- mean(linearData$ici)
  aicLinear <- mean(linearData$aic)
  
  
  rcs3 <- lapply(
    folds,
    evaluatePerformance,
    data = gusto,
    settings = createRcsSettings(nKnots = 3)
  )
  
  rcs3Data <- do.call(bind_rows, rcs3)
  cRcs3    <- mean(rcs3Data$c)
  iciRcs3  <- mean(rcs3Data$ici)
  aicRcs3  <- mean(rcs3Data$aic)
  
  
  rcs4 <- lapply(
    folds,
    evaluatePerformance,
    data = gusto,
    settings = createRcsSettings(nKnots = 4)
  )
  
  rcs4Data <- do.call(bind_rows, rcs4)
  cRcs4    <- mean(rcs4Data$c)
  iciRcs4  <- mean(rcs4Data$ici)
  aicRcs4  <- mean(rcs4Data$aic)
  
  
  rcs5 <- lapply(
    folds,
    evaluatePerformance,
    data = gusto,
    settings = createRcsSettings(nKnots = 5)
  )
  
  rcs5Data <- do.call(bind_rows, rcs5)
  cRcs5    <- mean(rcs5Data$c)
  iciRcs5  <- mean(rcs5Data$ici)
  aicRcs5  <- mean(rcs5Data$aic)
  
  tibble(
    discrimination = c(
      cConstant, clinear, cRcs3,
      cRcs4, cRcs5
    ),
    calibration = c(
      iciConstant, iciLinear, iciRcs3,
      iciRcs4, iciRcs5
    ),
    aic = c(
      aicConstnat, aicLinear, aicRcs3,
      aicRcs4, aicRcs5
    ),
    method = c(
      "constant treatment effect",
      "linear interaction",
      "RCS smoothing with 3 knots",
      "RCS smoothing with 4 knots",
      "RCS smoothing with 5 knots"
    )
  )
}

res <- list()

i <- 1
while (i <= 100) {
  res[[i]] <- tryCatch(
    {
      replicationExpression(gusto)
    },
    error = function(e) {
      e$message
    }
  )
  if (is.character(res[[i]])) {
    i <- i - 1
  } else {
    i <- i + 1
  }
}

do.call(bind_rows, res) %>%
  group_by(method) %>%
  summarise(
    discrimination = mean(discrimination),
    calibration = mean(calibration),
    aic = mean(aic)
  ) %>% 
  readr::write_csv("data/processed/gustoPerformanceMetrics.csv")

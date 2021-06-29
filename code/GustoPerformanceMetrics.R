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

set.seed(19910930)
load("data/raw/gusto.rda")

res <- list()

gusto <- gusto %>%
  tibble() %>%
  filter(!is.na(tpa)) %>%
  rename(
    "outcome" = "day30",
    "treatment" = "tpa"
  )

prediction <- lrm(
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
    riskLinearPredictor = riskLinearPredictor,
    predictedRisk       = plogis(riskLinearPredictor)
  )

constantModel <- fitModelBasedHte(
  data     = gusto, 
  settings = createModelBasedSettings(type = "treatment")
)

tmpData <- gusto %>%
  mutate(
    predictedBenefit = predictBenefitModelBasedHte(
      p = predictedRisk,
      modelBasedFit = constantModel
    )
  )

res$constant <- list(
  c = calculateCForBenefit(tmpData),
  ici = calculateCalibrationForBenefit(tmpData)$ici
)

stratified <- fitStratifiedHte(
  data     = gusto,
  settings = createStratifiedSettings()
)

tmpData <- gusto %>%
  mutate(
    predictedBenefit = predictStratifiedBenefit(
      p             = predictedRisk,
      stratifiedHte = stratified
    )
  )

res$stratified <- list(
  c = calculateCForBenefit(tmpData),
  ici = calculateCalibrationForBenefit(tmpData)$ici
)

linearModel <- fitModelBasedHte(
  data     = gusto,
  settings = createModelBasedSettings()
)

tmpData <- gusto %>%
  mutate(
    predictedBenefit = predictBenefitModelBasedHte(
      p             = predictedRisk,
      modelBasedFit = linearModel
    )
  )

res$linear <- list(
  c = calculateCForBenefit(tmpData),
  ici = calculateCalibrationForBenefit(tmpData)$ici
)

rcs3Model <- fitRcsHte(
  data     = gusto,
  settings = createRcsSettings()
)

tmpData <- gusto %>%
  mutate(
    predictedBenefit = predictSmoothBenefit(
      p         = predictedRisk,
      smoothFit = rcs3Model
    )
  )

res$rcs3 <- list(
  c = calculateCForBenefit(tmpData),
  ici = calculateCalibrationForBenefit(tmpData)$ici
)

rcs4Model <- fitRcsHte(
  data     = gusto,
  settings = createRcsSettings(nKnots = 4)
)

tmpData <- gusto %>%
  mutate(
    predictedBenefit = predictSmoothBenefit(
      p         = predictedRisk,
      smoothFit = rcs4Model
    )
  )

res$rcs4 <- list(
  c = calculateCForBenefit(tmpData),
  ici = calculateCalibrationForBenefit(tmpData)$ici
)

rcs5Model <- fitRcsHte(
  data     = gusto,
  settings = createRcsSettings(nKnots = 5)
)

tmpData <- gusto %>%
  mutate(
    predictedBenefit = predictSmoothBenefit(
      p         = predictedRisk,
      smoothFit = rcs5Model
    )
  )

res$rcs5 <- list(
  c = calculateCForBenefit(tmpData),
  ici = calculateCalibrationForBenefit(tmpData)$ici
)

 data.frame(
  matrix(
    unlist(res),
    nrow=length(res),
    byrow=TRUE
    )
) %>%
  tibble() %>%
  rename(
    "c"   = "X1",
    "ici" = "X2"
  ) %>%
   mutate(
     model = c(
       "Constant treatment effect",
       "Stratified",
       "Linear interaction",
       "RCS (3 knots)",
       "RCS (4 knots)",
       "RCS (5 knots)"
     )
   ) %>%
   relocate(
     model,
     .before = c
   ) %>%
   write_csv("data/processed/gustoPerformanceMetrics.csv")


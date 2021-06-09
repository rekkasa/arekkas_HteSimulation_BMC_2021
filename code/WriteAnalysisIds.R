#!/usr/bin/env Rscript

library(tidyverse)
library(readr)

effect       <- c("moderate", "high")
typeOfEffect <- c("constant", "linear", "quadratic")
n            <- c("4250", "1064", "17000")
auc          <- c("75", "65", "85")

grid1 <- expand.grid(
  type       = typeOfEffect,
  effectSize = effect,
  sampleSize = n,
  auc        = auc
) %>%
  arrange(type, effectSize, sampleSize, auc)

effect       <- "absent"
typeOfEffect <- "constant"

grid2 <- expand.grid(
  type       = typeOfEffect,
  effectSize = effect,
  sampleSize = n,
  auc        = auc
) %>%
  arrange(type, effectSize, sampleSize, auc)

table <- bind_rows(grid2, grid1) %>%
  mutate(
    b0 = case_when(
      auc == "75" ~ -2.08,
      auc == "65" ~ -1.63,
      auc == "85" ~ -2.7
    ),
    b1 = case_when(
      auc == "75" ~ .49,
      auc == "65" ~ .26,
      auc == "85" ~ .82
    ),
    b2 = b1,
    b3 = b1,
    b4 = b1,
    b5 = b1,
    b6 = b1,
    b7 = b1,
    b8 = b1,
    g0 = case_when(
      (type == "constant"   &  effectSize == "absent"                )   ~ 0,
      (type == "constant"   &  effectSize == "moderate"              )   ~ log(.8),
      (type == "constant"   &  effectSize == "high"                  )   ~ log(.5),
      (type == "linear"     &  effectSize == "moderate" & auc == "75")   ~ -.29,
      (type == "linear"     &  effectSize == "moderate" & auc == "65")   ~ -.35,
      (type == "linear"     &  effectSize == "moderate" & auc == "85")   ~ -.37,
      (type == "linear"     &  effectSize == "high"     & auc == "75")   ~ -.44,
      (type == "linear"     &  effectSize == "high"     & auc == "65")   ~ -.54,
      (type == "linear"     &  effectSize == "high"     & auc == "85")   ~ -.50,
      (type == "quadratic"  &  effectSize == "moderate" & auc == "75")   ~ -5.02,
      (type == "quadratic"  &  effectSize == "moderate" & auc == "65")   ~ -5.02,
      (type == "quadratic"  &  effectSize == "moderate" & auc == "85")   ~ -5.02,
      (type == "quadratic"  &  effectSize == "high"     & auc == "75")   ~ -4.42,
      (type == "quadratic"  &  effectSize == "high"     & auc == "65")   ~ -4.42,
      (type == "quadratic"  &  effectSize == "high"     & auc == "85")   ~ -4.42,
    ),
    g1 = case_when(
      (type == "constant"   |  type       == "quadratic")   ~ 1,
      (type == "linear"     &  effectSize == "moderate" & auc == "75")   ~ 0.9472527,
      (type == "linear"     &  effectSize == "moderate" & auc == "65")   ~ 0.9340659,
      (type == "linear"     &  effectSize == "moderate" & auc == "85")   ~ 0.9296703,
      (type == "linear"     &  effectSize == "high"     & auc == "75")   ~ 0.7956044,
      (type == "linear"     &  effectSize == "high"     & auc == "65")   ~ 0.7758242,
      (type == "linear"     &  effectSize == "high"     & auc == "85")   ~ 0.7846154
    ),
    g2 = case_when(
      (type == "constant"   |  type       == "linear"                )   ~ 0,
      (type == "quadratic"  &  effectSize == "moderate" & auc == "75")   ~ -0.01255887,
      (type == "quadratic"  &  effectSize == "moderate" & auc == "65")   ~ -0.01642314,
      (type == "quadratic"  &  effectSize == "moderate" & auc == "85")   ~ -0.01642314,
      (type == "quadratic"  &  effectSize == "high"     & auc == "75")   ~ -0.05168458,
      (type == "quadratic"  &  effectSize == "high"     & auc == "65")   ~ -0.05941311,
      (type == "quadratic"  &  effectSize == "high"     & auc == "85")   ~ -0.05941311
    ),
    c = case_when(
      (type == "linear"     |  type       == "constant")   ~ 0,
      (type == "quadratic"                             )   ~ -5
    )
  ) %>%
  mutate(scenario = 1:n()) %>%
  select(scenario, everything())

# saveRDS(table, "data/processed/analysisIds.rds")
readr::write_csv(table, "data/processed/analysisIds.csv")



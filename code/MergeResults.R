#!/usr/bin/env Rscript

library(tidyverse)
source("code/helpers/TmpFiles.R")

scenarios <- list.files("data/raw", pattern = "scenario", full.names = T)
if (!dir.exists(".scratch/tmp")) {
  dir.create(".scratch/tmp", recursive = TRUE)
}
lapply(scenarios, createTmpFiles, tmpDir = ".scratch/tmp/")
lapply(
  c("rmse", "discrimination", "calibration", "concordance", "adaptiveModel"), 
  mergeTmpFiles, 
  tmpDir = ".scratch/tmp",
  saveDir = "data/processed"
  )

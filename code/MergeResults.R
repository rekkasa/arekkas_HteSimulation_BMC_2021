#!/usr/bin/env Rscript

library(tidyverse)
source("code/helpers/TmpFiles.R")

scenarios <- length(list.files("data/raw", pattern = "scenario"))
if (!dir.exists(".scratch/tmp")) {
  dir.create(".scratch/tmp", recursive = TRUE)
}
lapply(1:scenarios, createTmpFiles, scenarioDir = "data/raw/", tmpDir = ".scratch/tmp/")
lapply(
  c("rmse", "discrimination", "calibration", "concordance"), 
  mergeTmpFiles, 
  tmpDir = ".scratch/tmp",
  saveDir = "data/processed"
  )

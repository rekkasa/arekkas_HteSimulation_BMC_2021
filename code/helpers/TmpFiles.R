library(tidyverse)

createTmpFiles <- function(scenarioDir, tmpDir) {
  
  evaluation <- readRDS(
    file.path(
      scenarioDir,
      "evaluation.rds"
    )
  )
  
  scenarioId <- as.numeric(str_extract(scenarioDir, "(?<=\\_).*"))
  
  evaluation <- evaluation %>% map(~mutate(., scenarioId = scenarioId))
  
  readr::write_csv(
    evaluation$rmse,
    file.path(
      tmpDir,
      paste(
        paste(
          "tmp",
          "rmse",
          "scenario",
          scenarioId,
          sep = "_"
        ),
        "csv",
        sep = "."
      )
    )
  )
  
  readr::write_csv(
    evaluation$discrimination,
    file.path(
      tmpDir,
      paste(
        paste(
          "tmp",
          "discrimination",
          "scenario",
          scenarioId,
          sep = "_"
        ),
        "csv",
        sep = "."
      )
    )
  )
  
  readr::write_csv(
    evaluation$calibration,
    file.path(
      tmpDir,
      paste(
        paste(
          "tmp",
          "calibration",
          "scenario",
          scenarioId,
          sep = "_"
        ),
        "csv",
        sep = "."
      )
    )
  )
  
  readr::write_csv(
    evaluation$calibration,
    file.path(
      tmpDir,
      paste(
        paste(
          "tmp",
          "concordance",
          "scenario",
          scenarioId,
          sep = "_"
        ),
        "csv",
        sep = "."
      )
    )
  )
  
  readr::write_csv(
    evaluation$adaptiveModel,
    file.path(
      tmpDir,
      paste(
        paste(
          "tmp",
          "adaptiveModel",
          "scenario",
          scenarioId,
          sep = "_"
        ),
        "csv",
        sep = "."
      )
    )
  )
  
  return("done")
  
}


mergeTmpFiles <- function(metric, tmpDir, saveDir) {
  files <- list.files(
    path=tmpDir,
    full.names = TRUE, 
    pattern = paste(
      "tmp",
      metric,
      sep = "_"
    )
  ) 
  
  defaultType <- ifelse(
    metric != "adaptiveModel",
    "d",
    "c"
  )
  
  tmp <- files %>%
    lapply(readr::read_csv, col_types = cols(.default = defaultType)) %>% 
    bind_rows %>%
    readr::write_csv(
      file.path(
        saveDir,
        paste(metric, "csv", sep = ".")
      )
    )
  
  file.remove(files)
  
  message(paste("Merged results for:", metric))
  
}

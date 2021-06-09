library(tidyverse)

createTmpFiles <- function(scenarioId, scenarioDir, tmpDir) {
  
  evaluation <- readRDS(
    file.path(
      scenarioDir,
      paste(
        "scenario",
        scenarioId,
        sep = "_"
      ),
      "evaluation.rds"
    )
  )
  
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
  
  tmp <- files %>%
    lapply(readr::read_csv, col_types = cols(.default = "d")) %>% 
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

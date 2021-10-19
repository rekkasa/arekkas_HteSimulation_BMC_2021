library(tidyverse)
rmse <- readr::read_csv(
  "data/rmse.csv",
  col_types = readr::cols(.default = "d")
)
discrimination <- readr::read_csv(
  "data/discrimination.csv",
  col_types = readr::cols(.default = "d")
)
calibration <- readr::read_csv(
  "data/calibration.csv",
  col_types = readr::cols(.default = "d")
)

analysisIds1 <- readr::read_csv(
  "data/analysisIds.csv"
) %>%
  select(scenario, base, type, harm, sampleSize, auc)

analysisIds2 <- readr::read_csv("data/analysisIdsInteractions.csv") %>%
  select(scenario, base, type, sampleSize, auc) %>%
  mutate(harm = "absent")

analysisIds <- bind_rows(analysisIds1, analysisIds2)




createPlot2 <- function(data) {
  tmp <- data %>%
    reshape2::melt()
  levels(tmp$variable) <- case_when(
    levels(tmp$variable) == "linear_predictor"          ~ "linear predictor",
    levels(tmp$variable) == "constant_treatment_effect" ~ "constant treatment\neffect",
    TRUE                                                ~ levels(tmp$variable)
  )
  plotly::plot_ly(
    x = ~ variable,
    y = ~ value,
    color = ~ variable,
    colors = c(
      "#1b9e77",
      "#d95f02",
      "#7570b3",
      "#e7298a",
      "#66a61e",
      "#e6ab02",
      "#a6761d"
    ),
    data = tmp
  ) %>%
    plotly::add_trace(
      type = "box",
      hoverinfo = "y+q"
    ) %>%
    plotly::layout(
      yaxis = list(
        hoverformat = ".3f",
        title = ""
      ),
      xaxis = list(
        title = ""
      )
    )
}


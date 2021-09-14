#!/usr/bin/env Rscript

# Description: 
# Output: 
# Depends: 

library(tidyverse)

source("code/helpers/CreateManuscriptPlots.R")

scenarios <- c(1, 10, 37, 55)

metric <- "rmse"
titles <- c(
  "**A.** Absent treatment effect",
  "**B.** Constant treatment effect",
  "**C.** Strong linear deviation",
  "**D.** Strong quadratic deviation"
)

metricFile <- paste(metric, "csv", sep = ".")

f <- function(x) x * 100

processed <- readr::read_csv(
  file = file.path("data/processed", metricFile)
) %>% 
  mutate_at(
    c(
      "constant_treatment_effect",
      "stratified",
      "linear_predictor",
      "rcs_3_knots",
      "rcs_4_knots",
      "rcs_5_knots",
      "adaptive"
    ),
    f
  )

plotList <- list()

for (i in seq_along(scenarios)) {
  tmp <- processed %>%
    dplyr::filter(scenarioId == scenarios[i])
  
  plot <- createPlot(
    data = tmp,
    metric = metric,
    title = titles[i],
    limits = c(0, 10),
    pointSize = .5
  ) +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x    = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(size = 10, angle = 45, hjust = 1),
      axis.title.y    = ggplot2::element_blank(),
      axis.text.y     = ggplot2::element_text(size = 9.5),
      plot.title      = ggtext::element_markdown(size = 14)
    )
  
  plotList[[i]] <- plot
}

pp <- gridExtra::grid.arrange(
  plotList[[1]],
  plotList[[2]],
  plotList[[3]],
  plotList[[4]],
  nrow = 2,
  ncol = 2,
  left = grid::textGrob(
    expression(
      paste(
        "Root mean squared error (x", 
        10^-2, 
        ")"
      )
    ),
    rot = 90
  )
)
  ggplot2::ggsave(
    file.path("figures", "rmse_base.tiff"), 
    plot = pp,
    dpi = 1200,
    width = 8.5, 
    height = 7,
    compression = "lzw"
  )
  
  ggplot2::ggsave(
    file.path("figures", "rmse_base.png"), 
    plot = pp,
    width = 8.5, 
    height = 7
  )
  
#!/usr/bin/env Rscript

# Description: 
# Output: 
# Depends: 

library(tidyverse)

source("code/helpers/CreateManuscriptPlots.R")

metric <- "rmse"
titles <- c(
  "Linear deviation",
  "Quadratic deviation"
)

metricFile <- paste(metric, "csv", sep = ".")

processed <- readr::read_csv(
  file = file.path("data/processed", metricFile)
) 

nonConstantIds <- c(37, 55)

for (i in seq_along(nonConstantIds)) {
  tmp <- processed %>%
    dplyr::filter(scenarioId == nonConstantIds[i])
  
  plotName <- paste(metric, nonConstantIds[i], sep = "_")
  
  plot <- createPlot(
    data = tmp,
    metric = metric,
    title = titles[i],
    limits = c(0, .08)
  )
  
  if (i > 1) {
    plot <- plot +
      ggplot2::theme(
        legend.position = "none",
        axis.title.x    = ggplot2::element_blank(),
        axis.title.y    = ggplot2::element_blank(),
        axis.text.x     = ggplot2::element_text(size = 16, angle = 45, hjust = 1),
        axis.text.y     = ggplot2::element_text(size = 16),
        plot.title      = ggplot2::element_text(size = 22)
      )
  } else {
    plot <- plot +
      ggplot2::theme(
        legend.position = "none",
        axis.title.x    = ggplot2::element_blank(),
        axis.title.y    = ggplot2::element_text(size = 18),
        axis.text.x     = ggplot2::element_text(size = 16, angle = 45, hjust = 1),
        axis.text.y     = ggplot2::element_text(size = 16),
        plot.title      = ggplot2::element_text(size = 22)
      )
  }
  
  ggplot2::ggsave(
    plot = plot,
    file.path("figures", paste0(plotName, ".tiff")),
    width  = 8.5,
    height = 7,
    device = "tiff",
    compression = "lzw"
  )
  
  ggplot2::ggsave(
    plot = plot,
    file.path("figures", paste0(plotName, ".png")),
    width  = 8.5,
    height = 7,
    device = "png"
  )
  
}

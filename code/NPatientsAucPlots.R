#!/usr/bin/env Rscript

# Description: 
# Output: 
# Depends: 

library(tidyverse)
library(ggtext)
library(glue)

source("code/helpers/CreateManuscriptPlots.R")

interactionIds <- c(59, 60, 62, 63)

metric <- "rmse"
titles <- c(
  "**A.** N: 1065, AUC: 0.65",
  "**B.** N: 1065, AUC: 0.85",
  "**C.** N: 17000, AUC: 0.65",
  "**D.** N: 17000, AUC: 0.85"
)

metricFile <- paste(metric, "csv", sep = ".")

processed <- readr::read_csv(
  file = file.path("data/processed", metricFile)
) 

plotList <- list()

for (i in seq_along(interactionIds)) {
  tmp <- processed %>%
    dplyr::filter(scenarioId == interactionIds[i])
  
  plot <- createPlot(
    data = tmp,
    metric = metric,
    title = titles[i],
    limits = c(0, .11),
    pointSize = .5
  ) 
  
  if (i %% 2 == 1)  {
    plot <- plot +
      ggplot2::theme(
        legend.position = "none",
        axis.title.x    = ggplot2::element_blank(),
        axis.text.x     = ggplot2::element_text(size = 9, angle = 45, hjust = 1),
        axis.title.y    = ggplot2::element_text(size = 11),
        axis.text.y     = ggplot2::element_text(size = 9),
        plot.title      = ggtext::element_markdown(size = 12)
      )
  } else {
    plot <- plot +
      ggplot2::theme(
        legend.position = "none",
        axis.title.x    = ggplot2::element_blank(),
        axis.text.x     = ggplot2::element_text(size = 9, angle = 45, hjust = 1),
        axis.title.y    = ggplot2::element_blank(),
        axis.text.y     = ggplot2::element_text(size = 9),
        plot.title      = ggtext::element_markdown(size = 12)
      )
  }
  
  plotList[[i]] <- plot
}

pp <- gridExtra::grid.arrange(
  plotList[[1]],
  plotList[[2]],
  plotList[[3]],
  plotList[[4]],
  nrow = 2,
  ncol = 2
)
  ggplot2::ggsave(
    file.path("figures", "rmse_n_auc.tiff"), 
    plot = pp,
    width = 8.5, 
    height = 7,
    compression = "lzw"
  )
  
  ggplot2::ggsave(
    file.path("figures", "rmse_n_auc.png"), 
    plot = pp,
    width = 8.5, 
    height = 7
  )
  
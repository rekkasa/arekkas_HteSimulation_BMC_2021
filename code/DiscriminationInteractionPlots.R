#!/usr/bin/env Rscript

# Description: 
# Output: 
# Depends: 

library(tidyverse)

source("code/helpers/CreateManuscriptPlots.R")

scenarios <- c(10, 64, 66, 65)

metric <- "discrimination"
titles <- c(
  "Base case scenario",
  "Weak interactions",
  "Mixed interactions",
  "Strong interactions"
)

metricFile <- paste(metric, "csv", sep = ".")

processed <- readr::read_csv(
  file = file.path("data/processed", metricFile)
) 

plotList <- list()

for (i in seq_along(scenarios)) {
  tmp <- processed %>%
    dplyr::filter(scenarioId == scenarios[i])
  
  plot <- createPlot(
    data = tmp,
    metric = metric,
    title = titles[i],
    limits = c(.52, .61),
    pointSize = .5
  ) +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x    = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(size = 9, angle = 45, hjust = 1),
      axis.title.y    = ggplot2::element_blank(),
      axis.text.y     = ggplot2::element_text(size = 9),
      plot.title      = ggtext::element_markdown(size = 12)
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
  left = "C-statistic for benefit"
)
  
  ggplot2::ggsave(
    file.path("figures", "discrimination_interactions.png"), 
    plot = pp,
    width = 11, 
    height = 10.5,
  )
  
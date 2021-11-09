#!/usr/bin/env Rscript

# Description: 
# Output: 
# Depends: 

library(tidyverse)
library(glue)
library(ggtext)
source("code/helpers/CreateManuscriptPlots.R")
source("code/helpers/PlotResult.R")

scenarioIds <- readr::read_csv("data/processed/analysisIds.csv") %>%
  filter(
    base == "moderate",
    !(type %in% c("linear-moderate", "quadratic-moderate")),
    sampleSize == 4250,
    auc == .75
  )
metric <- "discrimination"
value  <- "base"

titles <- scenarioIds %>%
  mutate(
    title = ifelse(
      type == "constant",
      str_to_sentence(glue("{str_replace_all(type, '-', ' ')} treatment effect")),
      str_to_sentence(glue("{str_replace_all(type, '-', ' ')} deviation"))
    )
  ) %>%
  select(title) %>%
  unlist() %>%
  unique()

names(titles) <- NULL

titlePrefix <- paste0(
  "**",
  LETTERS[1:4],
  ".**"
)

titles <- paste(
  titlePrefix,
  titles
)


metricFile <- paste(metric, "csv", sep = ".")

processed <- readr::read_csv(
  file = file.path("data/processed", metricFile)
)
scenarios <- scenarioIds %>%
  filter(harm == "absent") %>%
  select(scenario) %>%
  unlist()
names(scenarios) <- NULL

plotList <- plotResult(scenarios, processed, titles, metric = metric, limits = c(.5, .57))

pp <- gridExtra::grid.arrange(
  plotList[[1]] +
    theme(
      plot.title = element_markdown(),
      axis.title = element_blank(),
      legend.direction = "horizontal",
      legend.position = c(.444, .93),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      axis.text.x = element_blank()
    ),
  plotList[[2]] +
    theme(
      plot.title = element_markdown(),
      axis.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_blank()
    ),
  plotList[[3]] +
    theme(
      plot.title = element_markdown(),
      axis.title = element_blank(),
      legend.position = "none"
    ),
  plotList[[4]] +
    theme(
      plot.title = element_markdown(),
      axis.title = element_blank(),
      legend.position = "none"
    ),
  heights = c(1, 1.05),
  nrow = 2,
  ncol = 2,
  left = "C-statistic for benefit"
)

fileName <- paste0(
  paste(
    metric,
    value,
    sep = "_"
  ),
  ".tiff"
)
  ggplot2::ggsave(
    file.path("figures", fileName), 
    plot = pp,
    dpi = 1200,
    width = 10, 
    height = 7,
    compression = "lzw"
  )

#!/usr/bin/env Rscript

library(tidyverse)

adaptiveModel <- readr::read_csv("data/processed/adaptiveModel.csv") %>%
  mutate(scenarioId = as.factor(scenarioId))

p <- ggplot2::ggplot(
  data = adaptiveModel,
  ggplot2::aes(x = scenarioId, fill = selectedAdaptiveModel)
) +
  ggplot2::geom_bar(position = "fill") +
  theme_classic() +
  ggplot2::theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 90, size = 8)
  ) 

ggsave(
  "figures/selectedModelAdaptive.tiff",
  p, 
  compression = "lzw", 
  width       = 600, 
  height      = 300,
  units       = "mm",
  dpi         = 300
)



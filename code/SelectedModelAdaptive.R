#!/usr/bin/env Rscript

library(tidyverse)
library(ggthemes)

args <- commandArgs(trailingOnly = TRUE)
args_base <- as.character(args[1])
args_sampleSize <- as.numeric(args[2])
args_auc <- as.numeric(args[3])
args_value <- as.character(args[4])

analysisIds <- read_csv("data/processed/analysisIds.csv")

scenarios <- analysisIds %>%
  filter(
    base == args_base,
    !(type %in% c("quadratic-moderate", "linear-moderate")),
    harm != "negative",
    sampleSize == args_sampleSize,
    auc == args_auc
  ) %>%
  pull(scenario)

adaptiveModel <- readr::read_csv("data/processed/adaptiveModel.csv") %>%
  filter(scenarioId %in% scenarios) %>%
  left_join(analysisIds, by = c("scenarioId" = "scenario")) %>%
  select(
    c("selectedAdaptiveModel", "type", "harm")
  ) %>%
  mutate(
    selectedAdaptiveModel = factor(
      selectedAdaptiveModel,
      levels = rev(
        c(
          "treatment",
          "risk",
          "RCS with 3 knots",
          "RCS with 4 knots",
          "RCS with 5 knots"
        )
      ),
      labels = rev(
        c(
          "Constant treatment effect",
          "Linear interaction",
          "RCS-3",
          "RCS-4",
          "RCS-5"
        )
      )
    ),
    type = factor(
      type,
      levels = c(
        "constant",
        "linear-high",
        "quadratic-high",
        "non-monotonic"
      ),
      labels = c(
        "Constant treatment effect",
        "Strong linear deviation",
        "Strong quadratic deviation",
        "Non-monotonic deviation"
      )
    )
  )

p <- ggplot2::ggplot(
  data = adaptiveModel,
  ggplot2::aes(x = harm, fill = selectedAdaptiveModel)
) +
  ggplot2::geom_bar(position = "fill") +
  scale_fill_manual(
    values = c(
      "#264653",
      "#2A9D8F",
      "#E9C46A",
      "#F4A261",
      "#E76F51"
    ),
    breaks = c(
      "Constant treatment effect",
      "Linear interaction",
      "RCS-3",
      "RCS-4",
      "RCS-5"
    )
  ) +
  facet_grid(~type) +
  scale_x_discrete(
    breaks = c(
      "absent",
      "moderate-positive",
      "strong-positive"
    ),
    labels = c(
      "Absent",
      "Moderate",
      "Strong"
    )
  ) +
  xlab("Treatment-related harm") +
  ylab("Model selection rate") +
  theme_classic() +
  theme(
    axis.title       =  element_text(size = 12),
    axis.text        =  element_text(size = 8),
    strip.text       =  element_text(size = 12),
    strip.background = element_blank() ,
    legend.title     = element_blank(),
    legend.text      = element_text(size = 12)
  ) 

fileToSave <- paste0(
  paste(
    "selected_model_adaptive",
    args_value,
    sep = "_"
  ),
  ".tiff"
)

ggsave(
  file.path("figures", fileToSave),
  p, 
  compression = "lzw", 
  width       = 297, 
  height      = 210,
  units       = "mm",
  dpi         = 300
)



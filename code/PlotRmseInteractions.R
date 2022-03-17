#!/usr/bin/env Rscript

# ===================================================
# Description:
#   Generates the RMSE plots for the base case
# Input:
#   - type
#   - value
# Output:
#   - figures/rmse_base.tiff
# Depends:
#   - data/processed/analysisIds.csv
#   - data/processed/rmse.csv
#   - code/helpers/CreateManuscriptPlots.R
#   - code/helpers/PlotResult.R
#   - code/helpers/Absolute.R
# ===================================================

library(tidyverse)
library(glue)
library(ggtext)
library(gridExtra)
library(grid)
library(ggside)

args <- commandArgs(trailingOnly = TRUE)
interactionType <- as.character(args[1])

if (interactionType == "positive") {
  scenarioLimits <- c(649, 660)
} else if (interactionType == "negative") {
  scenarioLimits <- c(661, 672)
} else {
  scenarioLimits <- c(673, 676)
}


# --------------------------------------
# Sourcing helper files:
#   1. Generates single boxplot
#   2. Generates the boxplot list
#   3. Generates the absolute plots
# --------------------------------------
source("code/helpers/CreateManuscriptPlots.R")     
source("code/helpers/PlotResult.R")                
source("code/helpers/Absolute.R")                  

scenarioIds <- readr::read_csv("data/processed/analysisIdsInteractions.csv") %>%
  filter(scenario >= scenarioLimits[1] & scenario <= scenarioLimits[2])

metric    <- "rmse"
titles <- scenarioIds %>%
  mutate(
    title = str_to_sentence(glue("{str_replace_all(type, '-', ' ')} interactions")
    )
  ) %>%
  select(title) %>%
  unlist() %>%
  unique()

if (interactionType != "combined") {
  titlePrefix <- paste0(
    "**",
    LETTERS[1:3],
    ".**"
  )
} else {
  titlePrefix <- ""
}

titles <- paste(
  titlePrefix,
  titles
)

metricFile <- paste(metric, "csv", sep = ".")

f <- function(x) x * 100

limitsHigh <- .15

processed <- readr::read_csv(
  file = file.path("data/processed", metricFile)
) %>% 
  filter(scenarioId >= scenarioLimits[1] & scenarioId <= scenarioLimits[2]) %>%
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

# ------------------------------------
# Create the absolute benefit plots
# ------------------------------------
absolutePlots <- scenarioIds %>%
  filter(harm != "negative") %>%
  group_by(type) %>%
  nest() %>%
  mutate(
    plot = map2(
      data,
      type,
      ~ plotAbsoluteBenefit(
      data = .x,
      projectDir = "~/Documents/Projects/arekkas_HteSimulation_XXXX_2021",
      type = .y
      )
    )
  )


scenarios <- scenarioIds %>%
  filter(harm == "absent") %>%
  select(scenario) %>%
  unlist()

names(scenarios) <- NULL

plotList <- plotResult(
  scenarios,
  processed,
  titles,
  metric = metric,
  limits = c(0, 10, 2.5)
)

if (interactionType == "combined") {
  plotList <- plotResult(
    scenarios,
    processed,
    titles,
    metric = metric,
    limits = c(7.5, 12.5, 1)
  )
  gridList <- list(
    plotList[[1]] +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(size = 9),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.direction = "horizontal",
        legend.title = element_text(size = 7.5),
        legend.text = element_text(size = 7),
        legend.position = c(.273, .97)
      ),
      absolutePlots$plot[[1]] +
        ggtitle("Simulated absolute benefit in treated patients") +
        xlim(c(0, .5)) +
        scale_y_continuous(
          position = "right",
          limits = c(-.058, limitsHigh),
          breaks = seq(-.05, limitsHigh, .05)
        ) +
        scale_color_manual(
          name = "Constant treatment-\n related harm",
          values = c(
            "#26547C",
            "#06D6A0",
            "#EF476F"
          )
        ) +
        theme_bw() +
        theme(
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          ggside.line = element_blank(),
          ggside.rect = element_blank(),
          ggside.axis.text = element_text(size = 8),
          ggside.axis.ticks.length = unit(0, "pt"),
          ggside.panel.scale = .07,
          plot.title = element_markdown(size = 9),
          legend.position = "none"
        )
  )
  pp <- cowplot::plot_grid(
    gridList[[1]],
    gridList[[2]],
    ncol = 2,
    rel_widths = c(1, .5)
  )
  left.grob <- grid::textGrob(
      expression(
        paste(
          "Root mean squared error (x",
          10^-2,
          ")"
        )
      ),
      rot = 90
  )
  
  right.grob <- grid::textGrob(
      "Absolute benefit",
      rot = 270
  )
  
  bottom.left.grob <- grid::textGrob(
    "Method",
    just = "center",
    gp = gpar(fontsize = 10)
  )
  
  bottom.right.grob <- grid::textGrob(
    "Baseline risk",
    just = "center",
    gp = gpar(fontsize = 10)
  )
  
  bottom.grob <- grid.arrange(
    bottom.left.grob,
    bottom.right.grob,
    nrow = 1,
    widths = c(2, 1)
  )
  res <- grid.arrange(
    arrangeGrob(
      pp,
      left = left.grob,
      right = right.grob,
      bottom = bottom.grob
    )
  )
} else {
  gridList <- list(
  plotList[[1]] +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_markdown(size = 9),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 8),
      legend.direction = "horizontal",
      legend.title = element_text(size = 7.5),
      legend.text = element_text(size = 7),
      legend.position = c(.273, .87)
    ),
    absolutePlots$plot[[1]] +
      ggtitle("Simulated absolute benefit in treated patients") +
      xlim(c(0, .5)) +
      scale_y_continuous(
        position = "right",
        limits = c(-.058, limitsHigh),
        breaks = seq(-.05, limitsHigh, .05)
    ) +
    scale_color_manual(
      name = "Constant treatment-\n related harm",
      values = c(
        "#26547C",
        "#06D6A0",
        "#EF476F"
      )
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_markdown(size = 9),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 8),
      ggside.line = element_blank(),
      ggside.rect = element_blank(),
      ggside.axis.text = element_blank(),
      ggside.axis.ticks.length = unit(0, "pt"),
      ggside.panel.scale = .07,
      legend.position = "none"
    ),
    plotList[[2]] + 
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(size = 9),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8),
        ggside.line = element_blank(),
        ggside.rect = element_blank(),
        ggside.axis.text = element_blank(),
        ggside.axis.ticks.length = unit(0, "pt"),
        ggside.panel.scale = .07,
        legend.position = "none"
    ),
    absolutePlots$plot[[2]] +
      ggtitle("True absolute benefit in treatment arm") +
      xlim(c(0, .5)) +
      scale_y_continuous(
        position = "right",
        limits = c(-.058, limitsHigh),
        breaks = seq(-.05, limitsHigh, .05)
    ) +
    scale_color_manual(
      name = "Constant treatment-\n related harm",
      values = c(
        "#26547C",
        "#06D6A0",
        "#EF476F"
      )
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 8),
      ggside.line = element_blank(),
      ggside.rect = element_blank(),
      ggside.axis.text = element_blank(),
      ggside.axis.ticks.length = unit(0, "pt"),
      ggside.panel.scale = .07,
      plot.title = element_markdown(size = 9, color = "white"),
      legend.position = "none"
    ),
    plotList[[3]] +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(size = 9),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 8),
        legend.position = "none"
    ),
    absolutePlots$plot[[3]] +
      ggtitle("True absolute benefit in treatment arm") +
      xlim(c(0, .5)) +
      scale_y_continuous(
        position = "right",
        limits = c(-.058, limitsHigh),
        breaks = seq(-.05, limitsHigh, .05)
      ) +
    scale_color_manual(
      name = "Constant treatment-\n related harm",
      values = c(
        "#26547C",
        "#06D6A0",
        "#EF476F"
      )
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 8),
      ggside.line = element_blank(),
      ggside.rect = element_blank(),
      ggside.axis.text = element_blank(),
      ggside.axis.ticks.length = unit(0, "pt"),
      ggside.panel.scale = .07,
      plot.title = element_markdown(size = 9, color = "white"),
      legend.position = "none"
    )
  )
  pp <- cowplot::plot_grid(
    gridList[[1]],
    gridList[[2]],
    gridList[[3]],
    gridList[[4]],
    gridList[[5]],
    gridList[[6]],
    ncol = 2,
    rel_widths = c(1, .5)
  )
  left.grob <- grid::textGrob(
      expression(
        paste(
          "Root mean squared error (x",
          10^-2,
          ")"
        )
      ),
      rot = 90
  )
  
  right.grob <- grid::textGrob(
      "Absolute benefit",
      rot = 270
  )
  
  bottom.left.grob <- grid::textGrob(
    "Method",
    just = "center",
    gp = gpar(fontsize = 10)
  )
  
  bottom.right.grob <- grid::textGrob(
    "Baseline risk",
    just = "center",
    gp = gpar(fontsize = 10)
  )
  
  bottom.grob <- grid.arrange(
    bottom.left.grob,
    bottom.right.grob,
    nrow = 1,
    widths = c(2, 1)
  )
  res <- grid.arrange(
    arrangeGrob(
      pp,
      left = left.grob,
      right = right.grob,
      bottom = bottom.grob
    )
  )
}


fileName <- paste0(
  paste(
    metric,
    "interaction",
    interactionType,
    sep = "_"
  ),
  ".tiff"
)
  ggplot2::ggsave(
    file.path("figures", fileName), 
    plot = res,
    dpi = 1200,
    width = 10, 
    height = 8,
    compression = "lzw"
  )
  

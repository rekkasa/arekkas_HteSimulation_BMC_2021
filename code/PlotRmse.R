#!/usr/bin/env Rscript

# --------------------------------------------------
# Description:
#   Generates the RMSE plots for the base case
# Output:
#   figures/rmse_base.tiff
# Depends:
#   data/processed/analysisIds.csv
#   data/processed/rmse.csv
#   code/helpers/CreateManuscriptPlots.R
#   code/helpers/PlotResult.R
#   code/helpers/Absolute.R
# --------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
args_sampleSize <- as.numeric(args[1])
args_auc <- as.numeric(args[2])
args_value <- as.character(args[3])

library(tidyverse)
library(glue)
library(ggtext)
library(gridExtra)
library(grid)

source("code/helpers/CreateManuscriptPlots.R")     # Geneartes single boxplot
source("code/helpers/PlotResult.R")                # Generates the boxplot list
source("code/helpers/Absolute.R")                  # Generates the absolute plots

scenarioIds <- readr::read_csv("data/processed/analysisIds.csv") %>%
  filter(
    base == "moderate",
    type != "quadratic-moderate",
    sampleSize == args_sampleSize,
    auc == args_auc
  ) 

metric    <- "rmse"

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
  LETTERS[1:5],
  ".**"
)

titles <- paste(
  titlePrefix,
  titles
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

# ----------------------------------
# Create the absolute benefit plots
# ----------------------------------
absolutePlots <- scenarioIds %>%
  filter(harm != "negative") %>%
  group_by(type) %>%
  nest() %>%
  mutate(
    plot = map2(
      data,
      type,
      ~ plotAbsoluteBenefit(
        data = .x
      )
    )
  )

# --------------------
# Create the boxplots
# --------------------
scenarios <- scenarioIds %>%
  filter(harm == "absent") %>%
  select(scenario) %>%
  unlist()

names(scenarios) <- NULL

plotList <- plotResult(scenarios, processed, titles, metric = metric)


# ------------------------------------
# Put them all together:
#   - create a list of plots
#   - combine the list with cowplot
# -----------------------------------
gridList <- list(
  plotList[[1]] +
    theme(
      plot.title = element_markdown(size = 9),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 8),
      legend.direction = "horizontal",
      legend.title = element_text(size = 7.5),
      legend.text = element_text(size = 7),
      legend.position = c(.334, .827)
    ),
  absolutePlots$plot[[1]] +
    ggtitle("Simulated absolute benefit in treated patients") +
    xlim(c(0, .5)) +
    scale_y_continuous(
      position = "right",
      limits = c(-.05, .15),
      breaks = seq(-.05, .15, .05)
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
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 8),
      plot.title = element_markdown(size = 9),
      legend.position = "none"
    ),
  plotList[[2]] + 
    theme(
      plot.title = element_markdown(size = 9),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 8),
      legend.position = "none"
    ),
  absolutePlots$plot[[2]] +
    ggtitle("True absolute benefit in treatment arm") +
    xlim(c(0, .5)) +
    scale_y_continuous(
      position = "right",
      limits = c(-.05, .15),
      breaks = seq(-.05, .15, .05)
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
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 8),
      plot.title = element_markdown(size = 9, color = "white"),
      legend.position = "none"
    ),
  plotList[[3]] +
    theme(
      plot.title = element_markdown(size = 9),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none"
    ),
  absolutePlots$plot[[3]] +
    ggtitle("True absolute benefit in treatment arm") +
    xlim(c(0, .5)) +
    scale_y_continuous(
      position = "right",
      limits = c(-.05, .15),
      breaks = seq(-.05, .15, .05)
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
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_blank(),
      plot.title = element_markdown(size = 9, color = "white"),
      legend.position = "none"
    ),
  plotList[[4]] + 
    theme(
      plot.title = element_markdown(size = 9),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none"
    ),
  absolutePlots$plot[[4]] +
    ggtitle("True absolute benefit in treatment arm") +
    xlim(c(0, .5)) +
    scale_y_continuous(
      position = "right",
      limits = c(-.05, .15),
      breaks = seq(-.05, .15, .05)
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
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 8),
      plot.title = element_markdown(size = 9, color = "white"),
      legend.position = "none"
    ),
  plotList[[5]] +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      legend.position = "none",
      plot.title = element_markdown(size = 9)
    ),
  absolutePlots$plot[[5]] +
    ggtitle("Simulated absolute benefit in treatment arm") +
    xlim(c(0, .5)) +
    scale_y_continuous(
      position = "right",
      limits = c(-.05, .15),
      breaks = seq(-.05, .15, .05)
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
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_blank(),
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
  gridList[[7]],
  gridList[[8]],
  gridList[[9]],
  gridList[[10]],
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


res <- grid.arrange(arrangeGrob(pp, left = left.grob, right = right.grob))

# pp <- gridExtra::grid.arrange(
#   plotList[[1]] + theme(axis.text.x = element_blank()),
#   plotList[[2]] + theme(legend.position = "none", axis.text.x = element_blank()),
#   plotList[[3]] + theme(legend.position = "none", axis.text.x = element_blank()),
#   plotList[[4]] + theme(legend.position = "none"),
#   plotList[[5]] + theme(legend.position = "none"),
#   absolutePlots$plot[[1]],
#   absolutePlots$plot[[2]],
#   absolutePlots$plot[[3]],
#   absolutePlots$plot[[4]],
#   absolutePlots$plot[[5]],
#   ncol = 2,
#   left = grid::textGrob(
#     expression(
#       paste(
#         "Root mean squared error (x",
#         10^-2,
#         ")"
#       )
#     ),
#     rot = 90
#   )
# )

fileName <- paste0(
  paste(
    metric,
    args_value,
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
  

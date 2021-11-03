#!/usr/bin/env Rscript

# Description: 
# Output: 
# Depends: 

library(tidyverse)
library(glue)
library(ggtext)
library(gridExtra)
library(grid)

source("code/helpers/PlotGammas.R")                # Used to generate the absolute plots
source("code/helpers/PlotDeviationsFunctions.R")   # Generates the absolute plot
source("code/helpers/CreateManuscriptPlots.R")     # Geneartes single boxplot
source("code/helpers/PlotResult.R")                # Generates the boxplot list

scenarioIds <- readr::read_csv("data/processed/analysisIds.csv") %>%
  filter(
    base == "moderate",
    type != "quadratic-moderate",
    sampleSize == 4250,
    auc == .75
  ) 

metric    <- "rmse"
value     <- "base"

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
  select(-one_of("locfit")) %>%
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


plotAbsoluteBenefit <- function(data) {
  calcSquare <- function(x, g0 = 0, g1 = 0, g2 = 0, l = 0) {
    ret <- g0 + g1 * (x - l) + g2 * (x - l)**2
    return(ret)
  }
  calcAbsoluteBenefit <- function(p, g0 = 0, g1 = 0, g2 = 0, l = 0, harm) {
    x <- log(p / (1 - p))
    sq <- calcSquare(x, g0, g1, g2, l)
    benefit <- plogis(x) - plogis(sq) - harm
    return(benefit)
  }
  harmSettings <- unique(data$harm)
  res <- ggplot()
  for (i in seq_along(harmSettings)) {
    harm <- case_when(
      harmSettings[i] == "moderate-positive" ~ data$averageTrueBenefit[1] / 4, 
      harmSettings[i] == "strong-positive"   ~ data$averageTrueBenefit[1] / 2, 
      harmSettings[i] == "negative"          ~ -data$averageTrueBenefit[1] / 4, 
      TRUE                                   ~ 0
    )
    args <- list(
      g0 = data$g0,
      g1 = data$g1,
      g2 = data$g2,
      l = data$c,
      harm = harm
    )
    label <- harmSettings[i]
    res <- res + ggplot2::stat_function(
      data = data.frame(x = c(.05, .95), label = label),
      ggplot2::aes(x = x, color = label, group = label),
      fun = calcAbsoluteBenefit,
      args = args
    )
  }
  return(res)
}

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


# ----------------------
# Put them all together
# ----------------------
gridList <- list(
  plotList[[1]] +
    theme(
      plot.title = element_markdown(size = 9),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 8),
      legend.title = element_text(size = 7.5),
      legend.text = element_text(size = 7),
      legend.position = c(.274, .827)
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
    value,
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
  

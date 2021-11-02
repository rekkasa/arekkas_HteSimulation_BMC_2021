#!/usr/bin/env Rscript

# Description: 
# Output: 
# Depends: 

library(tidyverse)
library(glue)

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
  LETTERS[1:4],
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


plotAbsoluteBenefit <- function(data, label) {
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
      data$harm == "moderate-positive" ~ data$averageTrueBenefit / 4, 
      data$harm == "strong-positive" ~ data$averageTrueBenefit / 4, 
      data$harm == "negative" ~ -data$averageTrueBenefit / 4, 
      TRUE                          ~ 0
    )
    args <- list(
      g0 = data$g0,
      g1 = data$g1,
      g2 = data$g2,
      l = data$c,
      harm = harm
    )
    res <- res + ggplot2::stat_function(
      data = data.frame(x = c(.05, .95)),
      ggplot2::aes(x = x, color = label),
      fun = calcAbsoluteBenefit,
      args = args
    )
    return(res)
  }
  
}

absolutePlots <- scenarioIds %>%
  group_by(type) %>%
  nest() %>%
  mutate(
    plot = map2(
      data,
      type,
      ~ plotAbsoluteBenefit(
        data = .x,
        label = .y
      )
    )
  )

scenarios <- scenarioIds %>%
  filter(harm == "absent") %>%
  select(scenario) %>%
  unlist()

names(scenarios) <- NULL

plotList <- plotResult(scenarios, processed, titles, metric = metric)

absolutePlot <- generateAbsolutePlot(
  analysisIds = scenarioIds,
  scenarios = scenarios,
  legend.position = "top",
  legend.title = element_blank(),
  legend.text = element_text(size = 14),
  axis.title = element_text(size = 12)
)

pp <- gridExtra::grid.arrange(
  absolutePlot,
  plotList[[1]] + theme(axis.text.x = element_blank()),
  plotList[[2]] + theme(legend.position = "none", axis.text.x = element_blank()),
  plotList[[3]] + theme(legend.position = "none", axis.text.x = element_blank()),
  plotList[[4]] + theme(legend.position = "none"),
  plotList[[5]] + theme(legend.position = "none"),
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
  

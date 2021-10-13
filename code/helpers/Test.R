source("code/helpers/PlotGammas.R")

library(tidyverse)
library(SmoothHte)
library(SimulateHte)
library(SimulationEvaluationHte)
library(dplyr)

analysisIds <- readr::read_csv(
  "data/processed/analysisIds.csv",
)

analysisIdsInteractions <- readr::read_csv(
  "data/processed/analysisIdsInteractions.csv",
  col_types = "icfiidddddddddddddd"
)
calcAbsoluteBenefit <- function(p, g0 = 0, g1 = 0, g2 = 0, l = 0) {
  x <- log(p / (1 - p))
  sq <- calcSquare(x, g0, g1, g2, l)
  benefit <- plogis(x) - plogis(sq)
  return(benefit)
}

plotAbsoluteBenefit <- function(g0 = 0, g1 = 1, g2 = 0, l = 0, label) {
  ggplot2::stat_function(
    data = data.frame(x = c(.05, .95)),
    ggplot2::aes(x = x, color = label),
    fun = calcAbsoluteBenefit,
    args = list(g0 = g0, g1 = g1, g2 = g2, l = l)
  )
  
}

calcGompertz <- function(x, a, b) {
  
  ret <- exp(-(a + b * x))
  ret <- exp(-ret)
  
  return(ret)
}

calcAbsoluteBenefitGomp <- function(p, b) {
  
  p1 <- p^exp(-b)
  benefit <- p - p1
  
  return(benefit)
}


plotAbsoluteBenefitGomp <- function(b, label) {
  ggplot2::stat_function(
    data = data.frame(x = c(.05, .95)),
    ggplot2::aes(x = x, color = label),
    fun = calcAbsoluteBenefitGomp,
    args = list(b = b)
  )
  
}

plotGompertz <- function(
  g0       = 0, 
  g1       = 1, 
  linetype = 1,
  label    = "gompertz"
) {
  ggplot2::stat_function(
    data = data.frame(x = c(-5, 2)),
    ggplot2::aes(x = x, color = label),
    fun = calcGompertz,
    args = list(a = g0, b = g1),
    linetype = linetype
  )
}

scenarios <- c(46, 55)

gammas <- list()
for (i in seq_along(scenarios)) {
  idSettings <- analysisIds %>%
    filter(scenario == scenarios[i])
  gammas[[i]] <- list(
    g0 = idSettings$g0,
    g1 = idSettings$g1,
    g2 = idSettings$g2,
    c  = idSettings$c
  )
}

names(gammas) <- c(
  "gammas_moderate",
  "gammas_high"
)

gammas <- list()
for (i in seq_along(scenarios)) {
  idSettings <- analysisIds %>%
    filter(scenario == scenarios[i])
  gammas[[i]] <- list(
    g0 = idSettings$g0,
    g1 = idSettings$g1,
    g2 = idSettings$g2,
    c  = idSettings$c
  )
}

names(gammas) <- c(
  "gammas_moderate",
  "gammas_high"
)

ggplot() +
  plotAbsoluteBenefit(
    log(.8), 
    g1 = 1, 
    l = 0, label = "base-case"
  ) +
  plotAbsoluteBenefit(
    g0    = gammas$gammas_moderate$g0,
    g1    = gammas$gammas_moderate$g1,
    g2    = gammas$gammas_moderate$g2,
    l     = gammas$gammas_moderate$c,
    label = "moderate"
  ) +
  plotAbsoluteBenefit(
    g0    = gammas$gammas_high$g0,
    g1    = gammas$gammas_high$g1,
    g2    = gammas$gammas_high$g2,
    l     = gammas$gammas_high$c,
    label = "high"
  )  +
  plotAbsoluteBenefitGomp(-.3, label = "Gompertz") +
  xlim(0, .5) +
  xlab("Baseline risk") +
  ylab("Absolute benefit") +
  theme_classic() +
  scale_color_manual(
    values = c("#66c2a5", "#fc8d62", "#8da0cb", "#cc0084"),
    breaks = c("base-case", "moderate", "high", "Gompertz")
  ) +
  theme(
    axis.text = element_text(size = 23),
    axis.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    legend.title = element_blank()
  ) 



calcGompertz <- function(x, a, b, c = 0) {
  
  ret <- exp(-(a + b * (x - c)))
  ret <- exp(-ret)
  
  return(ret)
}



plotRelativeConstant <- function(gamma, label = "constant") {
  logit <- function(x) log(x / (1 - x))
  prob <- function(p, gamma) {
    (exp(logit(p) + gamma)) / (1 + exp(logit(p) + gamma))
  }
 
  ggplot2::stat_function(
    data = data.frame(x = c(0, .95)),
    ggplot2::aes(x = x, color = label),
    fun = prob,
    args = list(gamma = gamma)
  )
   
}


plotRelativeBenefitGomp <- function(b, harm = .01, label) {
  
  ggplot2::stat_function(
    data = data.frame(x = c(0, .95)),
    ggplot2::aes(x = x, color = label),
    fun = function(x, b, harm) x^exp(-b) + harm,
    args = list(b = b, harm = harm)
  )
  
}


plotRelativeRiskOld <- function(gammas, label) {
  
  g0 <- gammas$g0
  g1 <- gammas$g1
  g2 <- gammas$g2
  c  <- gammas$c
  
  calcRiskTreatment <- function(p, g0, g1, g2, c) {
   logit <- function(p) log(p / (1 - p))
   f <- function(x, g0, g1, g2, c) {
     g0 + g1 * (x - c) + g2 * (x - c)**2
   }
    u <- exp(f(x = logit(p), g0 = g0, g1 = g1, g2 = g2, c = c)) 
    u / (1 + u)
  }
  ggplot2::stat_function(
    data = data.frame(x = c(0, .95)),
    ggplot2::aes(x = x, color = label),
    fun = calcRiskTreatment,
    args = list(
      g0 = g0,
      g1 = g1,
      g2 = g2,
      c  = c
    )
  )
  
}

scenarios <- c(46, 55)
gammas <- list()
for (i in seq_along(scenarios)) {
  idSettings <- analysisIds %>%
    filter(scenario == scenarios[i])
  gammas[[i]] <- list(
    g0 = idSettings$g0,
    g1 = idSettings$g1,
    g2 = idSettings$g2,
    c  = idSettings$c
  )
}

ggplot() +
  plotRelativeConstant(0, "absent") +
  plotRelativeConstant(log(.5)) +
  plotRelativeBenefitGomp(b = -.1, harm = .01, label = "gompertz") +
  plotRelativeRiskOld(gammas = gammas[[1]], "moderate") +
  plotRelativeRiskOld(gammas = gammas[[2]], "strong") +
  xlim(0, .5)
e
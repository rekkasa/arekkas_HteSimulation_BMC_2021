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

# b1 -> first intersection with diagonal
# b2 -> second intersection with diagonal
# m  -> maximum distance from the diagonal in (b1, b2)
# c is set to 0 !!
calculateGammas <- function(b1, b2, m) {
  
  g2 <- -m / (b1 * b2 - (b1+b2)**2 / 2)
  g1 <- 1 - g2 * (b1 + b2)
  g0 <- -m + g2 * (b1 + b2)**2 / 2
  c  <- 0
  
  return(
    list(
      g0 = g0,
      g1 = g1,
      g2 = g2,
      c = c
    )
  )
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



plotRelativeConstant <- function(gamma, label = "constant", ...) {
  logit <- function(x) log(x / (1 - x))
  prob <- function(p, gamma) {
    (exp(logit(p) + gamma)) / (1 + exp(logit(p) + gamma))
  }
 
  ggplot2::stat_function(
    data = data.frame(x = c(0, .95)),
    ggplot2::aes(x = x, color = label),
    fun = prob,
    args = list(gamma = gamma),
    ...
  )
   
}


plotRelativeBenefitGomp <- function(fun, harm = .01, label, ...) {
  
  ggplot2::stat_function(
    data = data.frame(x = c(0, .95)),
    ggplot2::aes(x = x, color = label),
    fun = function(x, f, harm) x^exp(-f(-log(-log(x)))) + harm,
    args = list(f = fun, harm = harm),
    ...
  )
  
}


plotRelativeRiskOld <- function(gammas, label, ...) {
  
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
    ),
    ...
  )
  
}

plotAbsoluteBenefitGomp <- function(fun, harm = .01, label, ...) {
  
  ggplot2::stat_function(
    data = data.frame(x = c(0, .95)),
    ggplot2::aes(x = x, color = label),
    fun = function(x, f, harm) x - (x^exp(-f(-log(-log(x)))) + harm),
    args = list(f = fun, harm = harm),
    ...
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

f <- function(a, b, c) {
  return(
    function(x) a + b * (x - c)
  )
}

gammasNonMonotonic <- calculateGammas(-5, .2, 1)
gammas[[3]]        <- gammasNonMonotonic

ggplot() +
  plotRelativeConstant(0, "absent", linetype = "longdash", size = 1.2) +
  plotRelativeConstant(log(.8)) +
  plotRelativeBenefitGomp(f(-.26, .2, -.8), harm = .02, label = "gompertz") +
  plotRelativeRiskOld(gammas = gammas[[1]], "quadratic-moderate") +
  plotRelativeRiskOld(gammas = gammas[[2]], "quadratic-high") +
  plotRelativeRiskOld(gammas = gammas[[3]], "non-monotonic") +
  xlim(0, .5) +
  theme_classic() +
  xlab("Baseline risk") +
  ylab("Risk with treatment") +
  scale_color_manual(
    values = c("black", "#66c2a5", "#fc8d62", "#8da0cb", "#e41a1c", "#a65628"),
    breaks = c("absent", "constant", "quadratic-moderate", "quadratichigh", "gompertz", "non-monotonic")
  ) +
  theme(
    axis.text    = element_text(size = 19),
    axis.title   = element_text(size = 21),
    legend.text  = element_text(size = 15),
    legend.title = element_blank()
  )

# #a65628 "test"

scenarios <- c(10, 28, 37, 55)


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

gammas[[5]] <- gammasNonMonotonic
names(gammas) <- c(
  "gammas_base_case",
  "gammas_linear_moderate",
  "gammas_linear_high",
  "gammas_quadratic_high",
  "gammas_non_monotonic"
)

ggplot() +
  plotAbsoluteBenefitGomp(f(-.26, .2, -.8), harm = 0, label = "gompertz") +
  plotAbsoluteBenefit(
    g0    = gammas[[1]]$g0,
    g1    = gammas[[1]]$g1,
    g2    = gammas[[1]]$g2,
    l     = gammas[[1]]$c,
    label = "base-case"
  ) +
  plotAbsoluteBenefit(
    g0    = gammas[[2]]$g0,
    g1    = gammas[[2]]$g1,
    g2    = gammas[[2]]$g2,
    l     = gammas[[2]]$c,
    label = "linear-moderate"
  ) +
  plotAbsoluteBenefit(
    g0    = gammas$gammas_linear_high$g0,
    g1    = gammas$gammas_linear_high$g1,
    g2    = gammas$gammas_linear_high$g2,
    l     = gammas$gammas_linear_high$c,
    label = "linear-strong"
  ) +
  plotAbsoluteBenefit(
    g0    = gammas$gammas_quadratic_high$g0,
    g1    = gammas$gammas_quadratic_high$g1,
    g2    = gammas$gammas_quadratic_high$g2,
    l     = gammas$gammas_quadratic_high$c,
    label = "quadratic-strong"
  ) +
  plotAbsoluteBenefit(
    g0    = gammas$gammas_non_monotonic$g0,
    g1    = gammas$gammas_non_monotonic$g1,
    g2    = gammas$gammas_non_monotonic$g2,
    l     = gammas$gammas_non_monotonic$c,
    label = "non-monotonic"
  ) +
  xlim(0, .5) +
  xlab("Baseline risk") +
  ylab("Absolute benefit") +
  theme_classic() +
  scale_color_manual(
    values = c("black", "#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f"),
    breaks = c(
      "no effect", "base-case", "linear-moderate", 
      "linear-strong", "quadratic-strong", "non-monotonic", "gompertz"
    )
  ) +
  theme(
    axis.text       = element_text(size = 10),
    axis.title      = element_text(size = 11),
    legend.position = "right",
    legend.title = element_blank()
  )

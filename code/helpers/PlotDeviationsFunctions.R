generatePlot <- function(scenarios) {
  
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
    "gammas_base_case",
    "gammas_linear_moderate",
    "gammas_linear_high",
    "gammas_quadratic_high"
  )
  
  p <- constantEffectPlot(or = .8, label = "base-case") +
    plotSquare(g0 = 0, g1 = 1, label = "no effect", linetype = "dashed") +
    plotSquare(
      g0    = gammas$gammas_linear_moderate$g0,
      g1    = gammas$gammas_linear_moderate$g1,
      g2    = gammas$gammas_linear_moderate$g2,
      l     = gammas$gammas_linear_moderate$c,
      label = "linear-moderate"
    ) +
    plotSquare(
      g0    = gammas$gammas_linear_high$g0,
      g1    = gammas$gammas_linear_high$g1,
      g2    = gammas$gammas_linear_high$g2,
      l     = gammas$gammas_linear_high$c,
      label = "linear-strong"
    ) +
    plotSquare(
      g0    = gammas$gammas_quadratic_high$g0,
      g1    = gammas$gammas_quadratic_high$g1,
      g2    = gammas$gammas_quadratic_high$g2,
      l     = gammas$gammas_quadratic_high$c,
      label = "quadratic-strong"
    ) +
    xlab("Linear predictor in control arm") +
    ylab("Linear predictor in treatment arm") +
    xlim(-5, 2) +
    scale_color_manual(
      values = c(
        "black",
        "#66c2a5",
        "#fc8d62",
        "#8da0cb",
        "#e78ac3"
      ),
      breaks = c(
        "no effect",
        "base-case",
        "linear-moderate",
        "linear-strong",
        "quadratic-strong"
      )
    ) +
    theme_classic() +
    theme(
      axis.text       = element_text(size = 10),
      axis.title      = element_text(size = 11),
      legend.text     = element_text(size = 11),
      legend.title    = element_blank(),
      legend.position = c(.8, .2)
    )
  
  return(p)
}


generateAbsolutePlot <- function(analysisIds, scenarios, ...) {
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
    "gammas_base_case",
    "gammas_linear_moderate",
    "gammas_linear_high",
    "gammas_quadratic_high",
    "gammas_non_monotonic"
  )
  
  ggplot() +
    plotAbsoluteBenefit(
      log(.8), 
      g1 = 1, 
      l = 0, label = "base-case"
      ) +
    plotAbsoluteBenefit(
      g0    = gammas$gammas_linear_moderate$g0,
      g1    = gammas$gammas_linear_moderate$g1,
      g2    = gammas$gammas_linear_moderate$g2,
      l     = gammas$gammas_linear_moderate$c,
      label = "base-case"
    ) +
    plotAbsoluteBenefit(
      g0    = gammas$gammas_linear_moderate$g0,
      g1    = gammas$gammas_linear_moderate$g1,
      g2    = gammas$gammas_linear_moderate$g2,
      l     = gammas$gammas_linear_moderate$c,
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
      values = c(
        "#7fc97f",
        "#beaed4",
        "#fdc086",
        "#386cb0",
        "#f0027f"
      ),
      breaks = c(
        "base-case",
        "linear-moderate",
        "linear-strong",
        "quadratic-strong",
        "non-monotonic"
      )
    ) +
    theme(...)
}


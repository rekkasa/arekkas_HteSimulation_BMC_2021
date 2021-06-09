calcSquare <- function(x, g0 = 0, g1 = 0, g2 = 0, l = 0) {
  ret <- g0 + g1 * (x - l) + g2 * (x - l)**2
  return(ret)
}

plotSquare <- function(
  g0       = 0, 
  g1       = 0, 
  g2       = 0,
  l        = 0,
  linetype = 1,
  label    = "Diverge"
) {
  ggplot2::stat_function(
    data = data.frame(x = c(-5, 2)),
    ggplot2::aes(x = x, color = label),
    fun = calcSquare,
    args = list(g0 = g0, g1 = g1, g2 = g2, l = l),
    linetype = linetype
  )
}

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

constantEffectPlot <- function(
  or,
  label = "constant"
) {
  p <- ggplot2::ggplot() +
    ggplot2::stat_function(
      data = data.frame(x = c(-5, 2)),
      ggplot2::aes(x = x, color = label),
      fun = calcSquare,
      args = list(g0 = log(or), g1 = 1)
    )
  return(p)
}

# # plot linear -----------
# gammas_moderate <- derive_gammas_linear(-.2, .04, -5, -.45, .8)
# # gammas_high     <- derive_gammas_linear(-.4, .06, -5, -.45, .8)
# gammas_high     <- derive_gammas_linear(-.8, .15, -5, -.45, .8)
# constantEffectPlot(or = .8, label = "base-case") +
#   plotSquare(g0 = 0, g1 = 1, label = "no effect", linetype = "dashed") +
#   plotSquare(
#     g0 = gammas_moderate$gamma_0,
#     g1 = gammas_moderate$gamma_1,
#     l = 0,
#     label = "moderate"
#   ) +
#   plotSquare(
#     g0 = gammas_high$gamma_0,
#     g1 = gammas_high$gamma_1,
#     l = 0,
#     label = "high"
#   ) +
#   xlab("lp_0") +
#   ylab("lp_1") +
#   # xlim(-5, 2) +
#   scale_color_manual(
#     values = c("black", "#66c2a5", "#fc8d62", "#8da0cb"),
#     breaks = c("no effect", "base-case", "moderate", "high")
#   ) +
#   theme_classic() +
#   theme(
#     axis.text    = element_text(size = 18),
#     axis.title   = element_text(size = 20),
#     legend.text  = element_text(size = 15),
#     legend.title = element_blank()
#   )

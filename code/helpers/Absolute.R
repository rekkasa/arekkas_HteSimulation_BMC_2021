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

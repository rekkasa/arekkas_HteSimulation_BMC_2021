plotAbsoluteBenefit <- function(data, projectDir = NULL, type) {
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
  validationDataset <- list()
  for (i in seq_along(harmSettings)) {
    scenarioSettings <- data %>%
      dplyr::filter(harm == harmSettings[i]) %>%
      dplyr::mutate(
        harmValue = case_when(
          harm == "moderate-positive" ~ .data$averageTrueBenefit[1] / 4, 
          harm == "strong-positive"   ~ .data$averageTrueBenefit[1] / 2, 
          harm == "negative"          ~ -.data$averageTrueBenefit[1] / 4, 
          TRUE                        ~ 0
        )
      )
    scenarioDir <- paste(
      "scenario",
      as.character(scenarioSettings$scenario),
      sep = "_"
    )
    args <- list(
      g0   = scenarioSettings$g0,
      g1   = scenarioSettings$g1,
      g2   = scenarioSettings$g2,
      l    = scenarioSettings$c,
      harm = scenarioSettings$harmValue
    )
    label <- harmSettings[i]
    res <- res +
      ggplot2::stat_function(
        data = data.frame(x = c(.05, .95), label = label),
        ggplot2::aes(x = x, color = label, group = label),
        fun = calcAbsoluteBenefit,
        args = args
      )
    if (!is.null(projectDir)) {
      settings <- readRDS(
        file.path(
          projectDir,
          "data",
          "raw",
          scenarioDir,
          "settings.rds"
        )
      )
      validationDatabaseSettings <- settings$simulationSettings$databaseSettings
      validationDatabaseSettings$numberOfObservations <- 1e5
      validationDataset[[i]] <- SimulateHte::runDataGeneration(
        databaseSettings        = validationDatabaseSettings,
        propensitySettings      = settings$simulationSettings$propensitySettings,
        baselineRiskSettings    = settings$simulationSettings$baselineRiskSettings,
        treatmentEffectSettings = settings$simulationSettings$treatmentEffectSettings
      )
    }
  }
  if (!is.null(projectDir)) {
    fullValidationDataset <- validationDataset %>%
      bind_rows(.id = "harmSetting")
    res <- res +
      # ggside::ggside(x.pos = "bottom") +
      ggside::ggside(y.pos = "left")
    if (type == "constant") {
      res <- res +
      ggside::geom_xsideboxplot(
        aes(x = probs),
        data = validationDataset[[1]] %>% mutate(probs = plogis(untreatedRiskLinearPredictor)),
        orientation = "y",
        outlier.shape = NA
      )
    }
      # ggside::geom_ysideboxplot(
      #   aes(y = trueBenefit, fill = harmSetting),
      #   data = fullValidationDataset,
      #   orientation = "x",
      #   outlier.shape = NA
      #   ) +
    res <- res +
      scale_ysidex_continuous() +
      scale_xsidey_continuous() +
      scale_fill_manual(
        values = c(
          "#26547C",
          "#06D6A0",
          "#EF476F"
        )
      ) 
  }
  return(res)
}

plotResult <- function(scenarios, processed, titles, metric, limits = c(0, 10)) {
  
  plotList <- list()
  for (i in seq_along(scenarios)) {
    tmpList <- list()
    tmpList$absent <- processed %>%
      dplyr::filter(scenarioId == scenarios[i])
    tmpList$moderate_positive <- processed %>%
      dplyr::filter(scenarioId == scenarios[i] + 1)
    tmpList$strong_positive <- processed %>%
      dplyr::filter(scenarioId == scenarios[i] + 2)
    
    tmpData <- bind_rows(tmpList, .id = "harm")
    
    plot <- createPlot(
      data = tmpData,
      metric = metric,
      title = titles[i],
      limits = limits,
      pointSize = .2
    )
    
    plotList[[i]] <- plot
  }
  
  return(plotList)
}


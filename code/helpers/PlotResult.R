plotResult <- function(scenarios, processed, titles, metric, limits = c(0, 10)) {
  
  plotList <- list()
  for (i in seq_along(scenarios)) {
    tmpList <- list()
    tmpList$absent <- processed %>%
      dplyr::filter(scenarioId == scenarios[i])
    tmpList$positive <- processed %>%
      dplyr::filter(scenarioId == scenarios[i] + 1)
    tmpList$negative <- processed %>%
      dplyr::filter(scenarioId == scenarios[i] + 2)
    
    tmpData <- bind_rows(tmpList, .id = "harm")
    
    plot <- createPlot(
      data = tmpData,
      metric = metric,
      title = titles[i],
      limits = limits,
      pointSize = .2
    ) +
      ggplot2::theme(
        legend.position = c(.405, .923),
        legend.title    = ggplot2::element_text(size = 10),
        legend.text     = ggplot2::element_text(size = 7),
        legend.direction = "horizontal",
        axis.title.x    = ggplot2::element_blank(),
        axis.text.x     = ggplot2::element_text(size = 9),
        axis.title.y    = ggplot2::element_blank(),
        axis.text.y     = ggplot2::element_text(size = 8.5),
        plot.title      = ggtext::element_markdown(size = 14)
      )
    
    plotList[[i]] <- plot
  }
  
  return(plotList)
}


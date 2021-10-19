
plotResult <- function(scenarios, processed, titles, metric) {
  
  plotList <- list()
  
  for (i in seq_along(scenarios)) {
    tmp <- processed %>%
      dplyr::filter(scenarioId == scenarios[i])
    
    plot <- createPlot(
      data = tmp,
      metric = metric,
      title = titles[i],
      limits = c(0, 10),
      pointSize = .5
    ) +
      ggplot2::theme(
        legend.position = "none",
        axis.title.x    = ggplot2::element_blank(),
        axis.text.x     = ggplot2::element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y    = ggplot2::element_blank(),
        axis.text.y     = ggplot2::element_text(size = 9.5),
        plot.title      = ggtext::element_markdown(size = 14)
      )
    
    plotList[[i]] <- plot
  }
  
  return(plotList)
}


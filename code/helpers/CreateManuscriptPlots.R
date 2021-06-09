createPlot <- function(
  data,
  metric,
  title,
  limits,
  pointSize = 1.5
) {
  
  yAxis <- dplyr::case_when(
    metric == "rmse"           ~ "RMSE",
    metric == "discrimination" ~ "Discrimination",
    metric == "calibration"    ~ "Calibration"
  )
  
  tmp <- data %>%
    dplyr::select(
      c(
        "constant_treatment_effect", 
        "stratified", 
        "linear_predictor", 
        "rcs_3_knots",
        "rcs_4_knots",
        "rcs_5_knots", 
        "adaptive"
      )
    ) %>% 
    reshape2::melt()
  levels(tmp$variable) <- case_when(
    levels(tmp$variable) == "linear_predictor"          ~ "Linear predictor",
    levels(tmp$variable) == "constant_treatment_effect" ~ "Constant\ntreatment effect",
    levels(tmp$variable) == "stratified"                ~ "Stratified",
    levels(tmp$variable) == "rcs_3_knots"               ~ "RCS\n(3 knots)",
    levels(tmp$variable) == "rcs_4_knots"               ~ "RCS\n(4 knots)",
    levels(tmp$variable) == "rcs_5_knots"               ~ "RCS\n(5 knots)",
    levels(tmp$variable) == "adaptive"                  ~ "Adaptive",
    TRUE                                                ~ levels(tmp$variable)
  )
  
  plot <- ggplot2::ggplot(
    data = tmp,
    ggplot2::aes(
      x    = variable, 
      y    = value, 
      fill = variable
    )
  ) +
    ggplot2::geom_boxplot(
      outlier.size = pointSize
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "#66c2a5",
        "#fc8d62", 
        "#8da0cb", 
        "#e78ac3", 
        "#a6d854", 
        "#ffd92f", 
        "#e5c494"
      ),
      breaks = c(
        "Constant\ntreatment effect", 
        "Stratified", 
        "Linear predictor", 
        "RCS\n(3 knots)",
        "RCS\n(4 knots)",
        "RCS\n(5 knots)", 
        "Adaptive"
        )
    ) +
    ggplot2::ylab(yAxis) +
    ggplot2::ylim(limits[1], limits[2]) +
    ggplot2::theme_bw()
  
  if (!missing(title)) {
    plot <- plot +
      ggtitle(title)
  }
  
  return(plot)
}

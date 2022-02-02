createPlot <- function(
  data,
  metric,
  title,
  limits,
  pointSize = 1
) {
  
  yAxis <- dplyr::case_when(
    metric == "rmse"           ~ "RMSE",
    metric == "discrimination" ~ "Discrimination",
    metric == "calibration"    ~ "Calibration"
  )
  
  tmp <- data %>%
    mutate(
      harm = factor(
        harm,
        levels = c(unique(.data$harm, ordered = TRUE))
      )
    ) %>%
    dplyr::select(
      c(
        "harm",
        "constant_treatment_effect", 
        "stratified", 
        "linear_predictor", 
        "rcs_3_knots",
        "rcs_4_knots",
        # "rcs_5_knots", 
        "adaptive"
      )
    ) %>% 
    reshape2::melt()
  levels(tmp$variable) <- case_when(
    levels(tmp$variable) == "linear_predictor"          ~ "Linear",
    levels(tmp$variable) == "constant_treatment_effect" ~ "Constant",
    levels(tmp$variable) == "stratified"                ~ "Stratified",
    levels(tmp$variable) == "rcs_3_knots"               ~ "RCS-3",
    levels(tmp$variable) == "rcs_4_knots"               ~ "RCS-4",
    # levels(tmp$variable) == "rcs_5_knots"               ~ "RCS-5",
    levels(tmp$variable) == "adaptive"                  ~ "Adaptive",
    TRUE                                                ~ levels(tmp$variable)
  )
  
  
  plot <- ggplot2::ggplot(
    data = tmp,
    ggplot2::aes(
      x    = variable, 
      y    = value, 
      fill = harm
    )
  ) +
    ggplot2::geom_boxplot(
      outlier.size = pointSize,
      width = .8,
      lwd = .4,
      fatten = .9
    ) +
    ggplot2::scale_fill_manual(
      name = "Constant treatment-\n related harm",
      values = c(
        "#26547C",
        "#06D6A0",
        "#EF476F"
      ),
      breaks = c(
        "absent",
        "moderate_positive",
        "strong_positive"
      ),
      labels = c(
        "absent",
        "moderate",
        "strong"
      )
    ) +
    ggplot2::ylab(yAxis) +
    ggplot2::scale_y_continuous(
               breaks = seq(limits[1], limits[2], limits[3]),
               limits = c(limits[1], limits[2])
             ) +
    # ggplot2::ylim(limits[1], limits[2]) +
    ggplot2::theme_bw()
  
  if (!missing(title)) {
    plot <- plot +
      ggtitle(title)
  }
  
  return(plot)
}

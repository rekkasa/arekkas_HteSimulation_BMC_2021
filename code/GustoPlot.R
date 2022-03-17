#!/usr/bin/env Rscript

library(tidyverse)
library(rms)
library(SmoothHte)

load("data/raw/gusto.rda")
maxRisk <- .3      # the maximum risk to be plotted

gusto <- gusto %>%
  tibble() %>%
  filter(tx != "SK+tPA") %>%
  rename(
    "outcome" = "day30",
    "treatment" = "tpa"
  )

prediction <- lrm(
  outcome ~ treatment + age + Killip + pmin(sysbp, 120) + lsp(pulse, 50) + pmi + miloc,
  data = gusto,
  maxit = 99
)

riskLinearPredictor <- predict(
  prediction,
  newdata = gusto %>% mutate(treatment = 0) %>% data.frame()
)

gusto <- gusto %>%
  mutate(
    riskLinearPredictor = riskLinearPredictor
  )

constantModel <- fitModelBasedHte(
  data     = gusto, 
  settings = createModelBasedSettings(type = "treatment")
)

stratified <- fitStratifiedHte(
  data     = gusto,
  settings = createStratifiedSettings()
)

linearModel <- fitModelBasedHte(
  data     = gusto,
  settings = createModelBasedSettings()
)

rcs3Model <- fitRcsHte(
  data     = gusto,
  settings = createRcsSettings()
)

plot <- ggplot() +
  geom_pointrange(
    data = stratified$data,
    aes(
      x     = meanRisk, 
      y     = estimate, 
      ymin  = lower, 
      ymax  = upper,
      color = "Stratified"
    ),
    key_glyph = "rect"
  ) +
  stat_function(
    data = data.frame(
      x     = c(0, maxRisk), 
      label = "Constant treatment effect"
    ),
    aes(
      x     = x, 
      color = label,
      linetype = label
    ),
    size = .8,
    alpha = .8,
    fun  = predictBenefitModelBasedHte,
    args = list(
      modelBasedFit = constantModel
    )
  ) +
  stat_function(
    data = data.frame(
      x = c(0, maxRisk), 
      label = "Linear interaction"
    ),
    aes(
      x     = x,
      linetype = label,
      color = label
    ),
    size = .65,
    fun  = predictBenefitModelBasedHte,
    args = list(
      modelBasedFit = linearModel
    )
  ) +
  stat_function(
    data = data.frame(
      x = c(0, maxRisk), 
      label = "RCS 3 knots"
    ),
    aes(
      x     = x,
      color = label,
      linetype = label
    ),
    size = .65,
    fun  = predictSmoothBenefit,
    args = list(
      smoothFit = rcs3Model
    )
  ) +
  scale_color_manual(
    values = c(
      "#2274A5", 
      "#969A97",
      "#273043",
      "#F15152"
    ),
    breaks = c(
      "Constant treatment effect",
      "Stratified",
      "Linear interaction",
      "RCS 3 knots"
    )
  ) +
  scale_linetype_manual(
    values = c("dashed", "solid", "solid"),
    breaks = c(
      "Constant treatment effect",
      "Linear interaction",
      "RCS 3 knots"
    ),
    guide = "none"
  ) +
  xlab("Baseline risk") +
  ylab("Predicted benefit") +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    axis.text.x = ggplot2::element_text(size = 12),
    axis.text.y = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 14),
    legend.text = element_text(size = 10),
    legend.position = c(.17, .85)
  )
  
 ggplot2::ggsave(
    file.path("figures", "gusto.tiff"), 
    plot = plot,
    dpi = 1200,
    width = 7, 
    height = 5,
    compression = "lzw"
  )
  

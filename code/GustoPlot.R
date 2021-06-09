#!/usr/bin/env Rscript

library(tidyverse)
library(rms)
library(SmoothHte)

load("data/raw/gusto.rda")
maxRisk <- .4      # the maximum risk to be plotted

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

rcs4Model <- fitRcsHte(
  data     = gusto,
  settings = createRcsSettings(nKnots = 4)
)

rcs5Model <- fitRcsHte(
  data     = gusto,
  settings = createRcsSettings(nKnots = 5)
)



plot <- ggplot() +
  stat_function(
    data = data.frame(
      x     = c(0, maxRisk), 
      label = "constant"
    ),
    aes(
      x     = x, 
      color = label
    ),
    fun  = predictBenefitModelBasedHte,
    args = list(
      modelBasedFit = constantModel
    )
  ) +
  geom_pointrange(
    data = stratified$data,
    aes(
      x     = meanRisk, 
      y     = estimate, 
      ymin  = lower, 
      ymax  = upper,
      color = "stratified"
    ),
    key_glyph = "rect"
  ) +
  stat_function(
    data = data.frame(
      x = c(0, maxRisk), 
      label = "linear predictor"
    ),
    aes(
      x     = x,
      color = label
    ),
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
      color = label
    ),
    fun  = predictSmoothBenefit,
    args = list(
      smoothFit = rcs3Model
    )
  ) +
  stat_function(
    data = data.frame(
      x = c(0, maxRisk), 
      label = "RCS 4 knots"
    ),
    aes(
      x     = x,
      color = label
    ),
    fun  = predictSmoothBenefit,
    args = list(
      smoothFit = rcs4Model
    )
  ) +
  stat_function(
    data = data.frame(
      x = c(0, maxRisk), 
      label = "RCS 5 knots"
    ),
    aes(
      x     = x,
      color = label
    ),
    fun  = predictSmoothBenefit,
    args = list(
      smoothFit = rcs5Model
    )
  ) +
  stat_function(
    data = data.frame(
      x = c(0, maxRisk), 
      label = "RCS 5 knots"
    ),
    aes(
      x     = x,
      color = label
    ),
    fun  = predictSmoothBenefit,
    args = list(
      smoothFit = rcs5Model
    )
  ) +
  scale_color_manual(
    values = c(
      "#66c2a5", 
      "#fc8d62",
      "#8da0cb",
      "#e78ac3",
      "#a6d854", 
      "#ffd92f"
    ),
    breaks = c(
      "constant",
      "stratified",
      "linear predictor",
      "RCS 3 knots",
      "RCS 4 knots",
      "RCS 5 knots"
    )
  ) +
  xlab("Baseline risk") +
  ylab("Predicted benefit") +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    axis.text.x = ggplot2::element_text(size = 12),
    axis.text.y = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = c(.9, .2)
  )
  
 ggplot2::ggsave(
    file.path("figures", "gusto.tiff"), 
    plot = plot,
    dpi = 1200,
    width = 7, 
    height = 5,
    compression = "lzw"
  )
  
  ggplot2::ggsave(
    file.path("figures", "gusto.png"), 
    plot = plot,
    width = 7, 
    height = 5
  )
  
getMedian <- function(data) {
  data %>%
  summarise(
    median_constant = median(constant_treatment_effect),
    median_stratified = median(stratified),
    median_linear = median(linear_predictor),
    median_rcs_3 = median(rcs_3_knots),
    median_rcs_4 = median(rcs_4_knots),
  )
}

getLower <- function(data) {
  data %>%
  summarise(
    lower_constant   = quantile(constant_treatment_effect, probs = .025),
    lower_stratified = quantile(stratified, probs = .025),
    lower_linear     = quantile(linear_predictor, probs = .025),
    lower_rcs_3      = quantile(rcs_3_knots, probs = .025),
    lower_rcs_4      = quantile(rcs_4_knots, probs = .025),
  )
}

getUpper <- function(data) {
  data %>%
  summarise(
    upper_constant   = quantile(constant_treatment_effect, probs = .975),
    upper_stratified = quantile(stratified, probs = .975),
    upper_linear     = quantile(linear_predictor, probs = .975),
    upper_rcs_3      = quantile(rcs_3_knots, probs = .975),
    upper_rcs_4      = quantile(rcs_4_knots, probs = .975),
  )
}

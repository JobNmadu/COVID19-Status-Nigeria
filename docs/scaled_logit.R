scaled_logit <- function(x, lower, upper) {
  log((x - lower) / (upper - x))
}

inv_scaled_logit <- function(x, lower, upper) {
  (upper - lower) * exp(x) / (1 + exp(x)) + lower
}

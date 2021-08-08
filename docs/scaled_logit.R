scaled_logit <- function(x, lower, upper) {
  log((x - lower) / (upper - x))
}

inv_scaled_logit <- function(x, lower, upper) {
  (upper - lower) * exp(x) / (1 + exp(x)) + lower
}

constrained_forecast <- function(Model, lower, upper) {
  F2 <- Model$upper
  F1 <- Model$lower
  F1l1 <- inv_scaled_logit(x = F1[, 1], lower = lower, upper = upper)
  F1l2 <- inv_scaled_logit(x = F1[, 2], lower = lower, upper = upper)
  F2u1 <- inv_scaled_logit(x = F2[, 1], lower = lower, upper = upper)
  F2u2 <- inv_scaled_logit(x = F2[, 2], lower = lower, upper = upper)
  
  results <- list("Lower80" = F1l1,
                  "Lower95" = F1l2,
                  "Upper80" = F2u1,
                  "Upper95" = F2u2)
  return(results)
}

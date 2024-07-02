# Compute the probability of each ordinal state
# given the cutpoints and the linear predictor
compute_ord_prob <- function(cutpoints,
                             eta) {
  cum_prob <- stats::plogis(cutpoints - eta)

  # Take difference of adjacent cumulative probabilities
  probabilities <- c(cum_prob, 1) - c(0, cum_prob)

  return(probabilities)
}

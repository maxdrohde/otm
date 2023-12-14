#' Compute a vector of transition probability from an OTM
#'
#' @description
#' Given the parameters from an ordinal transition model (OTM),
#' calculate the transition probabilities for a given time and previous state.
#'
#' @param params A named list containing the parameters of the OTM.
#' Must include "beta_yprev": k-1 parameters for the k previous states,
#' "beta_regression": parameters for each covariates,
#' "beta_time": a single slope for the time parameter, and
#' "cutpoints": the cutpoint parameters.
#' @param yprev The previous state to be conditioned on.
#' @param day The day to compute the transition probabilities for.
#' @param covariates A vector of covariates
#' @return A vector of transition probabilities.
#' @export
compute_transition_probabilities <- function(params,
                                             yprev,
                                             day,
                                             covariates) {
  # Set the effect of previous state
  if (yprev == 1) {
    yprev_effect <- 0
  } else{
    yprev_effect <- params$beta_yprev[[yprev - 1]]
  }

  # Format covariates as a column vector
  covariates <- matrix(covariates, ncol=1)

  # Compute linear predictor
  eta <-
    as.numeric(((params$beta_regression %*% covariates) +
      (params$beta_time * day) +
      (yprev_effect)))

  # Compute transition probabilities
  probabilities <- stats::plogis(params$cutpoints - eta)
  probabilities <- c(probabilities, 1) - c(0, probabilities)

  return(probabilities)
}

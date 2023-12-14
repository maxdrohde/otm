#' Generate OTM data for a single subject
#'
#' @param params A named list containing the parameters of the OTM. Must include "beta_yprev": k-1 parameters for the k previous states,
#' "beta_regression": parameters for each covariates, "beta_time": a single slope for the time parameter, and
#' "cutpoints": the cutpoint parameters.
#' @param covariates A vector of covariates
#' @param baseline_y The baseline state of the subjects used when generating the data
#' @param times A vector of times to generate data for
#' @param absorb Which state is absorbing?
#' @return A data frame of data generated under the first order OTM model.
#' @export
generate_record <- function(params,
                            covariates,
                            baseline_y,
                            times,
                            absorb,
                            id){

  # Define the numbers of y states and the number of yprev states
  n_y <- length(params$cutpoints) + 1
  n_yprev <- length(params$beta_yprev) + 1

  y_levels <- 1:n_y

  yprev_states <- y_levels[-absorb]

  # Store the simulated y values
  y_stored <- integer()
  yprev_stored <- integer()

  # Counter for the number of iterations
  i <- 1

  # Set day to the first time
  day <- times[[i]]

  # Baseline state
  yprev <- baseline_y

  while (TRUE) {

    # Calculate the transition probabilities from the fitted model
    transition_probabilities <-
      compute_transition_probabilities(params = params,
                                       yprev = yprev,
                                       day = day,
                                       covariates = covariates)

    # Move to the next state according to the transition probabilities
    y <- sample(x = y_levels,
                size = 1,
                prob = transition_probabilities)

    # Save the value of y
    y_stored <- c(y_stored, y)
    yprev_stored <- c(yprev_stored, yprev)

    # Increment counter
    i <- i + 1

    # Break condition
    if ((i > length(times)) || (y == absorb)) {
      break
    }

    # Set next day
    day <- times[[i]]
    yprev <- y
  }

  out <- data.frame(day = times[1:length(y_stored)],
                    y = y_stored,
                    yprev = yprev_stored,
                    id = id)

  return(out)
}

#' Generate a dataset of OTM
#'
#' @param params A named list containing the parameters of the OTM. Must include "beta_yprev": k-1 parameters for the k previous states,
#' "beta_regression": parameters for each covariates, "beta_time": a single slope for the time parameter, and
#' "cutpoints": the cutpoint parameters.
#' @param covariates A vector of covariates
#' @param baseline_y The baseline state of the subjects used when generating the data
#' @param times A vector of times to generate data for
#' @param absorb Which state is absorbing?
#' @param n_subjects How many subject records should be generated?
#' @return A data frame of data generated under the first order OTM model.
#' @export
generate_dataset <-
  function(params,
           covariates,
           baseline_y,
           times,
           absorb,
           n_subjects){

    m <- purrr::map(1:n_subjects, ~generate_record(params = params,
                                                    covariates = covariates,
                                                    baseline_y = baseline_y,
                                                    times = times,
                                                    absorb = absorb,
                                                    id = .x),
                    .progress = FALSE)

    m <- data.table::rbindlist(m)

    return(m)
  }

#' Generate a dataset of OTM data with C++
#'
#' @param params A named list containing the parameters of the OTM. Must include "beta_yprev": k-1 parameters for the k previous states,
#' "beta_regression": parameters for each covariates, "beta_time": a single slope for the time parameter, and
#' "cutpoints": the cutpoint parameters.
#' @param covariates A vector of covariates
#' @param baseline_y The baseline state of the subjects used when generating the data
#' @param times A vector of times to generate data for
#' @param absorb Which state is absorbing?
#' @param n_subjects How many subject records should be generated?
#' @return A data frame of data generated under the first order OTM model.
#' @export
generate_dataset <-
  function(params,
           covariates,
           baseline_y,
           times,
           absorb,
           n_subjects){

  m <- purrr::map(1:n_subjects, ~generate_record(params = params,
                                                covariates = covariates,
                                                baseline_y = baseline_y,
                                                times = times,
                                                absorb = absorb,
                                                id = .x),
                  .progress = FALSE)

  m <- data.table::rbindlist(m)

  return(m)
}

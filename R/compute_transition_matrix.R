#' Compute the transition matrix implied by the OTM parameters on a given day
#'
#' @param params A named list containing the parameters of the OTM.
#' Must include "beta_yprev": k-1 parameters for the k previous states,
#' "beta_regression": parameters for each covariates,
#' "beta_time": a single slope for the time parameter, and
#' "cutpoints": the cutpoint parameters.
#' @param day Study day
#' @param covariates A vector of covariates
#' @param absorb Is the last y state an absorbing state?
#' @return Matrix of transition probabilities
#' @export
compute_transition_matrix <-
  function(params,
           day,
           covariates,
           absorb){

    # Define the numbers of y states and the number of yprev states
    n_y <- length(params$cutpoints) + 1
    n_yprev <- length(params$beta_yprev) + 1

    # Create a list of transition probabilities
    # Each element corresponds to a yprev state
    m <- purrr::map(1:n_yprev,
                    ~compute_transition_probabilities(params,
                                                      .x,
                                                      day,
                                                      covariates))

    # Bind the rows into a transition matrix
    m <- do.call(cbind, m)

    # Add absorbing column if specified
    if (absorb == TRUE) {
      # Create row for the absorbing state
      absorb_column <- rep(0L, n_y)
      absorb_column[[absorb]] <- 1L

      # Add a column for the absorbing state
      m <- cbind(m,
                 absorb_column,
                 deparse.level = 0)
    }

    return(m)
  }

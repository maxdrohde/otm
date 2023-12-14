#' Generate the SOP matrix implied by the OTM parameters
#'
#' @param params A named list containing the parameters of the OTM.
#' Must include "beta_yprev": k-1 parameters for the k previous states,
#' "beta_regression": parameters for each covariates,
#' "beta_time": a single slope for the time parameter, and
#' "cutpoints": the cutpoint parameters.
#' @param covariates A vector of covariates
#' @param baseline_y The baseline state of the subject
#' @param times A vector of times to generate data for
#' @param absorb Which state is absorbing?
#' @param output_type Either "df" or "matrix"
#' @return SOPs as a data frame or matrix.
#' @export
compute_sop <-
  function(params,
           covariates,
           baseline_y,
           times,
           absorb,
           output_type = "df"){

    n_y <- length(params$cutpoints) + 1

    l <- purrr::map(
      times,
      ~ compute_transition_matrix
      (
        params = params,
        day = .x,
        covariates = covariates,
        absorb = absorb
      )
    )

    sop <-
      SOP_from_matrix_list(
        l,
        baseline_state = baseline_y,
        output_type = output_type
      )

    return(sop)
  }

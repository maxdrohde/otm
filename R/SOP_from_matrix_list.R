#' Compute an SOP from a list of transition matrices
#'
#' @description
#' Let the transition matrices be denoted P1, P2, ... Pn for times 1 to n.
#'
#' The SOP at time j, starting from a baseline state of k, is P_t * P_(t-1) * ... * P_2 * SOP_(t = 1).
#'
#' SOP_(t = 1) is the kth column of P_1.
#'
#'
#' @param list_of_matrices A list of transition matrices, one for each time.
#' @param baseline_state The baseline state that the SOP is conditioned on.
#' @param output_type What format should be used for the output? Either "matrix" or "df".
#' @return Either a matrix or data.frame containing the SOPs
#' @export
#'
#' @examples
#' \dontrun{
#' SOP_from_matrix_list(l, baseline_state = 4, output_type = "matrix")
#' }
SOP_from_matrix_list <-
  function(list_of_matrices,
           baseline_state,
           output_type = "df"){

  # Number of transition matrices (one for each time point)
  n <- length(list_of_matrices)

  # Container to store the results of the recursive multiplications
  l <- vector(mode = "list", length = n)

  # The SOP at time 1 starting from a baseline state of k is
  # the kth column of P1
  l[[1]] <-
    list_of_matrices[[1]][, baseline_state] |>
    matrix(ncol = 1)

  # The SOP at time t is P_t * P_(t-1) * ... P_2 * SOP_(t = 1)
  for (i in 2:n) {
    l[[i]] <- list_of_matrices[[i]] %*% l[[i-1]]
  }

  # Bind the columns into a matrix
  sop <- do.call(cbind, l)

  if (output_type == "matrix") {
    return(sop)
  }

  if (output_type == "df") {

    sop <- reshape2::melt(sop)
    names(sop) <- c("y", "day", "sop")

    return(sop)
  }

  stop("Incorrect value supplied for `output_type`")
}


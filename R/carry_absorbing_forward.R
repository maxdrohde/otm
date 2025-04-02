#' Carry absorbing state forward for ordinal longitudinal data
#'
#' @param df Input data frame
#' @param absorb Which state is absorbing? Integer.
#' @param tmax What is the last day of the trial? Integer.
#' @return Data frame with absorbing state carried forward
#' @export
#'
carry_absorbing_forward <- function(df, absorb, tmax){

  df <- otm_verify(df)

  # If the absorbing state is not present in the data,
  # return the original data frame
  if (!(absorb %in% df$y)) {
      return(df)
  }

  # Get ids for participants with an absorbing state
  ids_absorb <-
    df |>
    dplyr::filter(y == absorb) |>
    dplyr::pull(id) |>
    unique()

  # For each participant with an absorbing state,
  # carry their absorbing state forward
  df_absorb <-
    df |>
    dplyr::filter(id %in% ids_absorb) |>
    dplyr::group_by(id) |>
    tidyr::complete(t = 1:tmax) |>
    tidyr::replace_na(list(y = absorb, yprev = absorb)) |>
    tidyr::fill(everything(), .direction = "down") |>
    dplyr::ungroup()

  # Merge with the rest of the subjects
  # and verify / sort the data
  df <-
    dplyr::bind_rows(df_absorb,
                     dplyr::filter(df, !(id %in% ids_absorb))) |>
    otm_verify()

  return(df)
}


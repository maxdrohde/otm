carry_absorbing_forward <- function(df, absorb, tmax){

  if (!(absorb %in% df$y)) {
      return(df)
  }

  df <- data.table::as.data.table(df)

  df[, ddeath := if(any(y == absorb)) min(t[y == absorb]) else NA_integer_, by=id]

  df_absorb <-
    df |>
    dplyr::mutate(y = as.integer(y),
           yprev = as.integer(yprev)) |>
    dplyr::filter(y == absorb) |>
    dplyr::group_by(id) |>
    tidyr::complete(t = ddeath:tmax, fill=list(y=absorb, yprev=absorb, gap=1)) |>
    dplyr::arrange(id, t) |>
    tidyr::fill(everything())

  df_other_states <-
    df |>
    dplyr::filter(y != absorb) |>
    dplyr::mutate(y = as.integer(y),
                  yprev = as.integer(yprev)) |>
    dplyr::arrange(id, t)

  df_forward <-
    dplyr::bind_rows(df_other_states, df_absorb) |>
    dplyr::mutate(y = as.integer(y),
           yprev = as.factor(yprev)) |>
    dplyr::arrange(id, t)

  return(df_forward)
}

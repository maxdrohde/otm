create_free_days <-
  function(
      df,
      bad_states,
      death_state) {

    df$bad <- df$y %in% c(bad_states)

    death_df <-
      df |>
        dplyr::summarize(died = death_state %in% y, .by = id)

    out <-
      df |>
      dplyr::summarise(free_days = sum(!bad),
                       .by = id)

    id_df <-
      df |>
      dplyr::filter(t == 1) |>
      dplyr::select(id, tx)

    out <- dplyr::inner_join(out, id_df, by = "id")
    out <- dplyr::inner_join(out, death_df, by = "id")

    out$free_days <- dplyr::if_else(out$died, -1, out$free_days)

    return(out)
}

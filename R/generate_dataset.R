#' TBD
#' @export
generate_dataset <-
  function(cutpoints,
           beta_yprev,
           beta_t,
           beta_tx,
           beta_t_tx,
           tx_end,
           baseline_y,
           times,
           tx_type,
           absorb,
           n_subjects){

    if (tx_type == "constant") stopifnot(beta_t_tx == 0)
    if (tx_type == "linear") stopifnot(beta_t_tx != 0)

    # Generate n_subjects number of trajectories
    # Treatment is allocated evenly between the participants
    # Each trajectory is a matrix
    m_list <- purrr::map2(1:n_subjects,
                          c(rep(1L,n_subjects / 2), rep(0L,n_subjects / 2)),
                          ~otm:::generate_trajectory(cutpoints = cutpoints,
                                               beta_yprev = beta_yprev,
                                               beta_t = beta_t,
                                               beta_tx = beta_tx,
                                               beta_t_tx = beta_t_tx,
                                               tx_end = tx_end,
                                               baseline_y = baseline_y,
                                               tx = .y,
                                               times = times,
                                               tx_type = tx_type,
                                               absorb = absorb,
                                               id = .x),
                          .progress = FALSE)

    # Combine the matrices into a data frame
    df <-
      do.call("rbind", m_list) |>
      as.data.frame()

    df$yprev <- as.factor(df$yprev)
    df$tx <- as.factor(df$tx)

    return(df)
  }

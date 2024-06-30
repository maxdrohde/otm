compute_transition_probabilities <- function(cutpoints,
                                             beta_yprev,
                                             beta_t,
                                             beta_tx,
                                             beta_t_tx,
                                             tx_type,
                                             tx_end,
                                             yprev,
                                             t,
                                             tx){

  # Use if treatment effect is constant over time
  if (tx_type == "constant") {
    stopifnot(beta_t_tx == 0)

    # Compute linear predictor
    eta <-
      ifelse(yprev == 1, 0, beta_yprev[[yprev - 1]]) +
      beta_tx * tx * ifelse(t <= tx_end, 1, 0) +
      beta_t * t
  }

  # Use if treatment effect changes linearly over time
  if (tx_type == "linear") {
    stopifnot(beta_t_tx != 0)

    # Compute linear predictor
    eta <-
      ifelse(yprev == 1, 0, beta_yprev[[yprev - 1]]) +
      beta_tx * tx * ifelse(t <= tx_end, 1, 0) +
      beta_t * t +
      beta_t_tx * tx * ifelse(t > tx_end, tx_end + 1, t)
  }

  # Compute transition probabilities
  probabilities <- stats::plogis(cutpoints - eta)
  probabilities <- c(probabilities, 1) - c(0, probabilities)

  return(probabilities)
}

generate_trajectory <- function(cutpoints,
                                beta_yprev,
                                beta_t,
                                beta_tx,
                                beta_t_tx,
                                tx_end,
                                baseline_y,
                                tx,
                                times,
                                tx_type,
                                absorb,
                                id){

    # Define the numbers of y states and the number of yprev states
    n_y <- length(cutpoints) + 1
    n_yprev <- length(beta_yprev) + 1
    y_levels <- 1:n_y
    yprev_states <- y_levels[-absorb]

    # Store the simulated y values
    t_max <- max(times)
    y_stored <- integer(t_max)
    yprev_stored <- integer(t_max)

    # Counter for the number of iterations
    i <- 1

    # Set day to the first time
    t <- times[[i]]

    # Baseline state
    yprev <- baseline_y

    while (TRUE) {

      # Calculate the transition probabilities from the fitted model
      transition_probabilities <-
        compute_transition_probabilities(cutpoints = cutpoints,
                                         beta_yprev = beta_yprev,
                                         beta_t = beta_t,
                                         beta_tx = beta_tx,
                                         beta_t_tx = beta_t_tx,
                                         tx_end = tx_end,
                                         yprev = yprev,
                                         t = t,
                                         tx = tx,
                                         tx_type = tx_type)

      # Move to the next state according to the transition probabilities
      y <- sample(x = y_levels,
                  size = 1,
                  prob = transition_probabilities)

      # Save the value of y
      y_stored[[i]] <- y
      yprev_stored[[i]] <- yprev

      # Increment counter
      i <- i + 1

      # Break condition
      if ((i > length(times)) || (y == absorb)) {
        break
      }

      # Set next day
      t <- times[[i]]
      yprev <- y
    }

    # Remove trailing zeros
    index <- match(0, y_stored) - 1
    if (!is.na(index)) {
      y_stored <- head(y_stored, index)
      yprev_stored <- head(yprev_stored, index)
    }

    out <- data.frame(t = times[1:length(y_stored)],
                      y = y_stored,
                      yprev = yprev_stored,
                      tx = tx,
                      id = id)

    return(out)
}

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

    m <- purrr::map2(1:n_subjects,
                     c(rep(1L,n_subjects / 2), rep(0L,n_subjects / 2)),
                     ~generate_trajectory(cutpoints = cutpoints,
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

    m <- data.table::rbindlist(m)

    m$yprev <- as.factor(m$yprev)
    m$tx <- as.factor(m$tx)
    return(m)
}

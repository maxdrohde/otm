# Generate a single trajectory from a OTM model
generate_otm_trajectory <-
  function(
    cutpoints,
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
    id) {

    n_y <- length(cutpoints) + 1
    n_yprev <- length(beta_yprev) + 1

    y_levels <- 1:n_y

    # Create the vectors ahead of time before filling in the values
    t_max <- max(times)
    y_stored <- integer(t_max)
    yprev_stored <- integer(t_max)

    # Counter for the number of iterations
    i <- 1

    # Initialize time to the first time point
    t <- times[[i]]
    # Initialize previous state to the baseline state
    yprev <- baseline_y

    while (TRUE) {

      # Calculate the linear predictor based on the
      # previous state, current time, and model parameters
      eta <-
        otm:::compute_eta(
          beta_yprev = beta_yprev,
          beta_t = beta_t,
          beta_tx = beta_tx,
          beta_t_tx = beta_t_tx,
          tx_end = tx_end,
          yprev = yprev,
          t = t,
          tx = tx,
          tx_type = tx_type)

      # Calculate the probabilities for each ordinal state
      trans_probs <-
        otm:::compute_ord_prob(cutpoints = cutpoints,
                               eta = eta)

      # Move to the next state according to the transition probabilities
      y <- sample(x = y_levels,
                  size = 1,
                  prob = trans_probs)

      # Store the current state and previous state for this iteration
      y_stored[[i]] <- y
      yprev_stored[[i]] <- yprev

      # Increment loop counter
      i <- i + 1

      # Break if the maximum time is reached
      # or an absorbing state is reached
      if ((i > length(times)) || (y == absorb)) {
        break
      }

      # Set time to the next time point
      t <- times[[i]]
      # Set the current state to be the next previous state
      yprev <- y
    }

    # Remove trailing zeros (from initializing the vector with zeros)
    index <- match(0, y_stored) - 1
    if (!is.na(index)) {
      y_stored <- head(y_stored, index)
      yprev_stored <- head(yprev_stored, index)
    }

    num_obs <- length(y_stored)

    trajectory <-
      cbind(t = times[1:num_obs],
            y = y_stored,
            yprev = yprev_stored,
            tx = tx,
            id = id)

    return(trajectory)
  }


# Generate data from an OTM model structure
generate_otm_data <-
  function(
      n_subjects,
      cutpoints,
      beta_yprev,
      beta_t,
      beta_tx,
      beta_t_tx,
      tx_end,
      baseline_y,
      times,
      tx_type,
      absorb) {

    if (tx_type == "constant") stopifnot(beta_t_tx == 0)
    if (tx_type == "linear") stopifnot(beta_tx == 0)
    if (tx_type == "linear_to_zero") stopifnot(beta_tx == 0)

    # Allocate half to treatment, half to placebo
    treatment_indicators <-
      c(rep(1L, n_subjects / 2),
        rep(0L, n_subjects / 2))

    # Generate n_subjects number of trajectories
    # Each trajectory is a matrix
    m_list <- purrr::map2(
      1:n_subjects,
      treatment_indicators,
      ~ otm:::generate_otm_trajectory(
        cutpoints = cutpoints,
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
        id = .x
      )
    )

    # Combine the matrices into a data frame
    df <-
      do.call("rbind", m_list) |>
      as.data.frame()

    df$yprev <- as.factor(df$yprev)
    df$tx <- as.factor(df$tx)

    return(df)
  }

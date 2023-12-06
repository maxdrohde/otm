compute_transition_matrix <-
  function(params,
           day,
           covariates,
           absorb){

    # Absorbing state must be the last state

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

    # Create row for the absorbing state
    absorb_column <- rep(0L, n_y)
    absorb_column[[absorb]] <- 1L

    # Add a column for the absorbing state
    m <- cbind(m,
               absorb_column,
               deparse.level = 0)

    return(m)
  }

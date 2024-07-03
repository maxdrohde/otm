generate_one_subject <- function(
    cutpoints,
    beta_t,
    beta_tx,
    beta_t_tx,
    tx,
    times,
    rand_intercept_sd,
    rand_slope_sd,
    rand_eff_corr,
    id) {
  # Number of states
  K <- as.integer(length(cutpoints) + 1)

  t <- c(0, times)
  M <- length(t)

  # Generate subject specific random effects
  sds <- diag(c(rand_intercept_sd, rand_slope_sd))
  corr <- matrix(c(1, rand_eff_corr, rand_eff_corr, 1), nrow = 2)
  sigma <- sds %*% corr %*% sds
  rand_effs <-
    MASS::mvrnorm(
      n = 1,
      mu = c(0, 0),
      Sigma = sigma
    )

  # Generate outcome
  etas <-
    beta_tx * tx +
    (beta_t + rand_effs[[2]]) * t +
    (beta_t_tx * t * tx) +
    rand_effs[[1]]


  y <- integer(M)
  for (i in 1L:M) {
    probabilities <- otm:::compute_ord_prob(cutpoints = cutpoints, eta = etas[[i]])
    y[[i]] <- sample(1L:K, size = 1, prob = probabilities)
  }

  trajectory <-
    cbind(
      t = t,
      y = y,
      tx = tx,
      id = id
    )

  return(trajectory)
}

generate_ordinal_random_effects_data <- function(N,
                                                 times,
                                                 cutpoints,
                                                 beta_tx,
                                                 beta_t,
                                                 beta_t_tx,
                                                 rand_intercept_sd,
                                                 rand_slope_sd,
                                                 rand_eff_corr){


# Assign equal numbers to treatment and control
tx_vector <- c(rep(0, N/2), rep(1, N/2))

l <- list(
     id = 1:N,
     tx = tx_vector)

m_list <-
  purrr::pmap(l, ~generate_one_subject(id = ..1,
                                      tx = ..2,
                                      cutpoints = cutpoints,
                                      times = times,
                                      beta_tx = beta_tx,
                                      beta_t = beta_t,
                                      beta_t_tx = beta_t_tx,
                                      rand_intercept_sd = rand_intercept_sd,
                                      rand_slope_sd = rand_slope_sd,
                                      rand_eff_corr = rand_eff_corr),
              .progress = TRUE)

  df <-
    do.call("rbind", m_list) |>
    as.data.frame()

  df$yprev <- dplyr::lag(df$y)

  df <- filter(df, t !=0)

  df$tx <- as.factor(df$tx)
  df$y <- as.ordered(df$y)

  return(df)
}

# df <-
#   generate_ordinal_random_effects_data(N = 30,
#                                        M = c(20),
#                                        cutpoints = c(-4, -3, -2, -1, 0, 2, 5),
#                                        beta_tx = -1,
#                                        beta_t = -0.3,
#                                        beta_t_tx = 0,
#                                        rand_intercept_sd = 1,
#                                        rand_slope_sd = 0.05,
#                                        rand_eff_corr = 0)

# df |>
#   ggplot() +
#   aes(x = t, y = y, group = id) +
#   geom_step() +
#   facet_wrap(~id)

# mod <- ordinal::clmm(formula = y ~ t + tx + (1 + t|id),
#                      control = ordinal::clmm.control(maxIter = 5000,
#                                                      maxLineIter = 200,
#                                                      useMatrix = TRUE),
#                      link = "logit",
#                      data = df)
#
# mod <- brms::brm(formula = y ~ t + tx + (1 + t|id),
#                  data = df,
#                  cores = 8,
#                  chains = 8,
#                  refresh = 10,
#                  family = brms::cumulative(),
#                  algorithm = "sampling",
#                  backend = "cmdstanr")

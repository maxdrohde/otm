generate_one_subject <- function(id,
                                 tx,
                                 M,
                                 cutpoints,
                                 beta_tx,
                                 beta_t,
                                 beta_interaction,
                                 rand_intercept_sd,
                                 rand_slope_sd,
                                 rand_eff_corr){

  K <- as.integer(length(cutpoints) + 1)

  # Generate subject specific random effects
  sds <- diag(c(rand_intercept_sd, rand_slope_sd))
  corr <- matrix(c(1, rand_eff_corr, rand_eff_corr, 1), nrow = 2)
  sigma <- sds %*% corr %*% sds
  rand_effs <-
    MASS::mvrnorm(n = 1,
                  mu = c(0,0),
                  Sigma = sigma)

  # Generate covariate values
  t <- 0:(M-1)

  # Generate outcome
  etas <-
    beta_tx * tx +
    (beta_t + rand_effs[[2]]) * t +
    (beta_interaction * t * tx) +
    rand_effs[[1]]


y <- integer(M)
for (i in 1L:M) {
  probabilities <- stats::plogis(cutpoints - etas[[i]])
  probabilities <- c(probabilities, 1) - c(0, probabilities)
  y[[i]] <- sample(1L:K, size = 1, prob = probabilities)
}

  return(data.frame(y=y,
                    t=t,
                    tx=tx,
                    rand_int = rand_effs[[1]],
                    rand_slope = rand_effs[[2]],
                    id=id))
}

generate_ordinal_random_effects_data <- function(N,
                                                 Ms,
                                                 cutpoints,
                                                 beta_tx,
                                                 beta_t,
                                                 beta_interaction,
                                                 rand_intercept_sd,
                                                 rand_slope_sd,
                                                 rand_eff_corr){


# Check length
stopifnot("N must be divisible by the length of Ms" = (N / length(Ms)) == as.integer(N / length(Ms)))

# Assign equal numbers to treatment and control
tx_vector <- c(rep(0, N/2), rep(1, N/2))

l <- list(
     id = 1:N,
     M = rep(Ms, each = N / length(Ms)),
     tx = tx_vector)

df <-
  purrr::pmap(l, ~generate_one_subject(id = ..1,
                                      M = ..2,
                                      tx = ..3,
                                      cutpoints = cutpoints,
                                      beta_tx = beta_tx,
                                      beta_t = beta_t,
                                      beta_interaction = beta_interaction,
                                      rand_intercept_sd = rand_intercept_sd,
                                      rand_slope_sd = rand_slope_sd,
                                      rand_eff_corr = rand_eff_corr),
              .progress = TRUE) |>
  purrr::list_rbind()

  df$y <- as.ordered(df$y)

  return(df)
}

# df <-
#   generate_ordinal_random_effects_data(N = 30,
#                                        M = c(20),
#                                        cutpoints = c(-4, -3, -2, -1, 0, 2, 5),
#                                        beta_tx = -1,
#                                        beta_t = -0.3,
#                                        beta_interaction = 0,
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

test_that("Mixed effect ordinal data generation", {
  if (FALSE) {
    df <-
      generate_ordinal_random_effects_data(n_subjects = 2000,
                                           times = 1:14,
                                           cutpoints = c(-2, -1, 0, 1),
                                           beta_tx = -1,
                                           beta_t = -0.05,
                                           beta_t_tx = -0.05,
                                           rand_intercept_sd = 1,
                                           rand_slope_sd = 0.1,
                                           rand_eff_corr = 0.3)

    mod <- brms::brm(formula = y ~ t + tx + (1 + t|id),
                     data = df,
                     cores = 6,
                     chains = 6,
                     refresh = 10,
                     iter = 1000,
                     family = brms::cumulative(),
                     algorithm = "sampling",
                     backend = "cmdstanr")
  }
})

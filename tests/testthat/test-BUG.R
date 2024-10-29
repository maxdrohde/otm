test_that("SYSTEM BUG?", {
  set.seed(777)

  df <-
    otm:::generate_ordinal_random_effects_data(n_subjects = 100,
                                               times = 1:28,
                                               cutpoints = c(-5, -3, -2, 0, 1, 2, 4),
                                               beta_tx = 0,
                                               beta_t = -0.2,
                                               beta_t_tx = 0,
                                               rand_intercept_sd = 1,
                                               rand_slope_sd = 1e-05,
                                               rand_eff_corr = 0)

  fit <- otm:::fit_rand_eff(df, 1, FALSE, TRUE)

  print(fit$anova)

  print("----------------------------")
  print("----------------------------")
  print("----------------------------")
  print("----------------------------")
  print("----------------------------")

  print(table(df$y))

  print(fit$full)

  print(fit$reduced)
})

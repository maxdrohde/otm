test_that("OTM data generation linear", {

  # True parameters
  cutpoints <- c(-2,-1,1,2,3)
  beta_yprev <- c(-2,-1,1,2,3)
  beta_t <- -0.02
  beta_tx <- 0
  beta_t_tx <- -0.02

  # Generate OTM Data
  df <-
  otm:::generate_otm_data(
    cutpoints = cutpoints,
    beta_yprev = beta_yprev,
    beta_t = beta_t,
    beta_tx = beta_tx,
    beta_t_tx = beta_t_tx,
    tx_end = 14,
    baseline_y = 4,
    times = 1:14,
    tx_type = "linear",
    absorb = 999L,
    n_subjects = 5000)

  mod_full <- VGAM::vglm(formula = y ~ yprev + t * tx,
                         family = VGAM::cumulative(parallel = TRUE),
                         data = df)

  # Need to flip some of the signs because of VGAM
  estimated_cutpoints <- as.numeric(mod_full@coefficients[1:5])
  estimated_beta_yprev <- -as.numeric(mod_full@coefficients[6:10])
  estimated_beta_t <- -as.numeric(mod_full@coefficients[11])
  estimated_beta_tx <- -as.numeric(mod_full@coefficients[12])
  estimated_beta_t_tx <- -as.numeric(mod_full@coefficients[13])

  if (TRUE) {
    cat("\n")
    cat("\n")
    cat("True vs Estimated Values\n")
    cat("------------------------\n")
    cat("Cutpoints\n")
    print(waldo::compare(estimated_cutpoints, cutpoints, x_arg = "Estimated", y_arg = "True"))
    cat("\n")
    cat("Previous State\n")
    print(waldo::compare(estimated_beta_yprev, beta_yprev, x_arg = "Estimated", y_arg = "True"))
    cat("\n")
    cat("Time\n")
    print(waldo::compare(estimated_beta_t, beta_t, x_arg = "Estimated", y_arg = "True"))
    cat("\n")
    cat("TX\n")
    print(waldo::compare(estimated_beta_tx, beta_tx, x_arg = "Estimated", y_arg = "True"))
    cat("\n")
    cat("TX by Time\n")
    print(waldo::compare(estimated_beta_t_tx, beta_t_tx, x_arg = "Estimated", y_arg = "True"))
    cat("\n")
  }

  # Assert that the values are within 5% error
  testthat::expect_equal(estimated_cutpoints, cutpoints, tolerance = 0.05)
  testthat::expect_equal(estimated_beta_yprev, beta_yprev, tolerance = 0.05)
  testthat::expect_equal(estimated_beta_t, beta_t, tolerance = 0.05)
  testthat::expect_equal(estimated_beta_tx, beta_tx, tolerance = 0.05)
  testthat::expect_equal(estimated_beta_t_tx, beta_t_tx, tolerance = 0.05)

})

test_that("OTM data generation constant", {

  # True parameters
  cutpoints <- c(-2,-1,1,2,3)
  beta_yprev <- c(-2,-1,1,2,3)
  beta_t <- -0.02
  beta_tx <- -2
  beta_t_tx <- 0

  # Generate OTM Data
  df <-
    otm:::generate_otm_data(
      cutpoints = cutpoints,
      beta_yprev = beta_yprev,
      beta_t = beta_t,
      beta_tx = beta_tx,
      beta_t_tx = beta_t_tx,
      tx_end = 14,
      baseline_y = 4,
      times = 1:14,
      tx_type = "constant",
      absorb = 999L,
      n_subjects = 5000)

  mod_full <- VGAM::vglm(formula = y ~ yprev + t * tx,
                         family = VGAM::cumulative(parallel = TRUE),
                         data = df)

  # Need to flip some of the signs because of VGAM
  estimated_cutpoints <- as.numeric(mod_full@coefficients[1:5])
  estimated_beta_yprev <- -as.numeric(mod_full@coefficients[6:10])
  estimated_beta_t <- -as.numeric(mod_full@coefficients[11])
  estimated_beta_tx <- -as.numeric(mod_full@coefficients[12])
  estimated_beta_t_tx <- -as.numeric(mod_full@coefficients[13])

  if (TRUE) {
    cat("\n")
    cat("\n")
    cat("True vs Estimated Values\n")
    cat("------------------------\n")
    cat("Cutpoints\n")
    print(waldo::compare(estimated_cutpoints, cutpoints, x_arg = "Estimated", y_arg = "True"))
    cat("\n")
    cat("Previous State\n")
    print(waldo::compare(estimated_beta_yprev, beta_yprev, x_arg = "Estimated", y_arg = "True"))
    cat("\n")
    cat("Time\n")
    print(waldo::compare(estimated_beta_t, beta_t, x_arg = "Estimated", y_arg = "True"))
    cat("\n")
    cat("TX\n")
    print(waldo::compare(estimated_beta_tx, beta_tx, x_arg = "Estimated", y_arg = "True"))
    cat("\n")
    cat("TX by Time\n")
    print(waldo::compare(estimated_beta_t_tx, beta_t_tx, x_arg = "Estimated", y_arg = "True"))
    cat("\n")
  }

  # Assert that the values are within 5% error
  testthat::expect_equal(estimated_cutpoints, cutpoints, tolerance = 0.05)
  testthat::expect_equal(estimated_beta_yprev, beta_yprev, tolerance = 0.05)
  testthat::expect_equal(estimated_beta_t, beta_t, tolerance = 0.05)
  testthat::expect_equal(estimated_beta_tx, beta_tx, tolerance = 0.05)
  testthat::expect_equal(estimated_beta_t_tx, beta_t_tx, tolerance = 0.05)

})

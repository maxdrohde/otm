test_that("Test `create_survival()`", {
  df <- data.frame(
    y = c(5,5,6,6,4,3,3,2,1,
          7,5,3,2,2,2,2,2,3),
    t = c(1:9, 1:9),
    tx = 1,
    id = c(1,1,1,1,1,1,1,1,1,
           2,2,2,2,2,2,2,2,2))

    recovery_states <- c(1L,2L,3L)
    surv_df <- otm:::create_survival(df,
                                     death_state = 8L,
                                     recovery_states = recovery_states,
                                     tmax = 9L)
    testthat::expect_equal(c(surv_df$time[[1]], surv_df$status[[1]]), c(6,1))
    testthat::expect_equal(c(surv_df$time[[2]], surv_df$status[[2]]), c(3,1))

    recovery_states <- c(4L)
    surv_df <- otm:::create_survival(df,
                                     death_state = 8L,
                                     recovery_states = recovery_states,
                                     tmax = 9L)
    testthat::expect_equal(c(surv_df$time[[1]], surv_df$status[[1]]), c(5,1))
    testthat::expect_equal(c(surv_df$time[[2]], surv_df$status[[2]]), c(9,0))

    df <- data.frame(
    y = c(5,5,6,6,4,8,
          7,5,3,2,2,2,2,2,3),
    t = c(1:6, 1:9),
    tx = 1,
    id = c(1,1,1,1,1,1,
           2,2,2,2,2,2,2,2,2))

    recovery_states <- c(1L,2L,3L)
    surv_df <- otm:::create_survival(df,
                                     death_state = 8L,
                                     recovery_states = recovery_states,
                                     tmax = 9L)
    testthat::expect_equal(c(surv_df$time[[1]], surv_df$status[[1]]), c(9,0))
    testthat::expect_equal(c(surv_df$time[[2]], surv_df$status[[2]]), c(3,1))
})

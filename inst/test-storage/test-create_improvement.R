test_that("Test `create_improvement()`", {
  df <- data.frame(
    y = c(c(5,6,5,5,5,5,2), c(5,6,2,5,5,5,5)),
    yprev = c(c(6,5,6,5,5,5,5), c(6,5,6,2,5,5,5)),
    t = c(1:7, 1:7),
    tx = 1,
    id = c(1,1,1,1,1,1,1,
           2,2,2,2,2,2,2))

    imp_df <- otm:::create_improvement(df,
                                     death_state = 8L,
                                     threshold = 2L,
                                     tmax = 7L)

    testthat::expect_equal(c(imp_df$time[[1]], imp_df$status[[1]]), c(7,1))
    testthat::expect_equal(c(imp_df$time[[2]], imp_df$status[[2]]), c(3,1))

    df <- data.frame(
      y = c(c(5,6,5,5,5,5,2), c(5,6,2,5,5,5,5)),
      yprev = c(c(1,5,6,5,5,5,5), c(2,5,6,2,5,5,5)),
      t = c(1:7, 1:7),
      tx = 1,
      id = c(1,1,1,1,1,1,1,
             2,2,2,2,2,2,2))

    imp_df <- otm:::create_improvement(df,
                                       death_state = 8L,
                                       threshold = 2L,
                                       tmax = 7L)

    testthat::expect_equal(c(imp_df$time[[1]], imp_df$status[[1]]), c(7,0))
    testthat::expect_equal(c(imp_df$time[[2]], imp_df$status[[2]]), c(7,0))

    df <- data.frame(
      y = c(c(5,6,5,5,3,5,2), c(5,6,5,5,3,5,2)),
      yprev = c(c(5,5,6,5,5,3,5), c(4,5,6,5,5,3,5)),
      t = c(1:7, 1:7),
      tx = 1,
      id = c(1,1,1,1,1,1,1,
             2,2,2,2,2,2,2))

    imp_df <- otm:::create_improvement(df,
                                       death_state = 8L,
                                       threshold = 2L,
                                       tmax = 7L)

    testthat::expect_equal(c(imp_df$time[[1]], imp_df$status[[1]]), c(5,1))
    testthat::expect_equal(c(imp_df$time[[2]], imp_df$status[[2]]), c(7,1))
})

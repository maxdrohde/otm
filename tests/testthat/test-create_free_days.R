test_that("Test `create_free_days()`", {
  df <- data.frame(
    y = c(1,2,2,3,4,4,5,6,7,
          1,2,3,4,4,4,3,2,1),
    t = c(1:9, 1:9),
    tx = 1,
    id = c(1,1,1,1,1,1,1,1,1,
           2,2,2,2,2,2,2,2,2))

  free_days <- otm:::create_free_days(df,
                                      bad_states = c(4,5,6),
                                      death_state = 8L)

  testthat::expect_equal(free_days$free_days[[1]], 5)
  testthat::expect_equal(free_days$free_days[[2]], 6)

  free_days <- otm:::create_free_days(df,
                                      bad_states = c(1,2),
                                      death_state = 8L)

  testthat::expect_equal(free_days$free_days[[1]], 6)
  testthat::expect_equal(free_days$free_days[[2]], 5)
})

test_that("Test `create_free_days() with absorbing state`", {
  df <- data.frame(
    y = c(1,2,2,3,4,8,
          1,2,3,4,4,4,3,2,8),
    t = c(1:6, 1:9),
    tx = 1,
    id = c(1,1,1,1,1,1,
           2,2,2,2,2,2,2,2,2))

  free_days <- otm:::create_free_days(df,
                                      bad_states = c(4,5,6,7),
                                      death_state = 8L)

  testthat::expect_equal(free_days$free_days[[1]], -1)
  testthat::expect_equal(free_days$free_days[[2]], -1)
})

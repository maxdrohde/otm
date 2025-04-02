test_that("carry_absorbing_forward() works", {

  test_df <-
    data.frame(
      t = c(1,2,1,1,2,3,1,2),
      id = c(1,1,2,3,3,3,4,4),
      y = c(5,6,6,3,4,4,3,6),
      yprev = c(4,5,4,3,3,4,3,3),
      tx = "A"
    )

  correct <-
    tibble::tibble(
      id = rep(seq(1, 4, by = 1), each = 3L),
      t = rep(1:3, 4),
      y = c(5L, 6L, 6L, 6L, 6L, 6L, 3L, 4L, 4L, 3L, 6L, 6L),
      yprev = c(4L, 5L, 6L, 4L, 6L, 6L, 3L, 3L, 4L, 3L, 3L, 6L),
      tx = "A"
    )

  testthat::expect_equal(carry_absorbing_forward(test_df, 6L, 3L),
                         correct)
})





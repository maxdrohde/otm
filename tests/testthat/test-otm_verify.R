test_that("otm::otm_verify() with missing columns", {
  test_df <- data.frame(y = c(1, 2), t = c(1,2), id = c(1,1))
  testthat::expect_error(otm_verify(test_df), "Columns: \\[yprev, tx\\] not present in data frame")
})

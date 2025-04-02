#' Verify ordinal longitudinal dataset
#'
#' This function:
#' - Checks that `id`, `y`, `yprev`, `t`, and `tx` columns are in the dataset
#' - Standardizes the data types for the columns
#' - Sorts the data by `id` and `t`
#' @param df data.frame to check
#' @return Modified data frame
#' @export
#'
otm_verify <- function(df){

  # Check that all required columns are present
  required_variables <- c("id", "y", "yprev", "t", "tx")
  booleans <- required_variables %in% names(df)
  if (!all(booleans)) {
    missing_variables_string <- paste(required_variables[!booleans], collapse = ", ")
    stop(glue::glue("Columns: [{missing_variables_string}] not present in data frame"))
  }

  # Standardize data types
  df$y <- as.integer(df$y)
  df$yprev <- as.integer(df$yprev)
  df$t <- as.integer(df$t)

  # Sort data by id then t
  df <-
    df |>
    dplyr::arrange(id, t)

  return(df)
}

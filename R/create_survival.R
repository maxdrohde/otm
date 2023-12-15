death_index <- function(y){
  index <- which(y == 8L)
  if (length(index) == 0L) return(NA_integer_)
  return(index)
}

recovery_index <- function(y){
  index <- which(y %in% c(1L,2L,3L))
  if (length(index) == 0L) return(NA_integer_)
  return(min(index))
}

assign_status <- function(id, death_day, last_day, recovery_day, tmax){

  stopifnot(is.integer(tmax))

  if (!is.na(recovery_day)) {
    status <- 1L
    time <- recovery_day
  } else if(!is.na(death_day)){
    status <- 0L
    time <- tmax
  } else{
    status <- 0L
    time <- last_day
  }
  return(c(id = id, time = time, status = status))
}

#' Generate the survival dataset from an OTM dataset
#'
#' @param df An OTM dataset generated with otm::generate_dataset
#' @param tmax The final day of the trial (used for censoring at death)
#' @return A survival dataset
#' @export
create_survival <- function(df, tmax){

  # Create a dataset that identifies for each subject
  # death day, last day, and recovery day
  surv_df <-
    df |>
    dplyr::summarise(death_day = day[death_index(y)],
                     last_day = max(day),
                     recovery_day = day[recovery_index(y)],
                     .by="id")
  surv_df <-
  surv_df |>
    purrr::transpose() |>
    map_dfr(~assign_status(
                       id = .x$id,
                       death_day = .x$death_day,
                       last_day = .x$last_day,
                       recovery_day = .x$recovery_day,
                       tmax = tmax))

  return(surv_df)
}

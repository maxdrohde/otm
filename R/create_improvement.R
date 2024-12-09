utils::globalVariables(c("y", "t"))

# Get the index that corresponds to death
# Return NA if not present
death_index <- function(y, death_state){
  index <- which(y == death_state)
  if (length(index) == 0L) return(NA_integer_)
  return(index)
}

# Get the first index that corresponds to improvement
# y0 is baseline state
# threshold is amount needed to define improvement
# Return NA if not present
improvement_index <- function(y, y0, threshold){

  recovery_states <- 0:(y0-threshold)

  index <- which(y %in% recovery_states)
  if (length(index) == 0L) return(NA_integer_)
  return(min(index))
}

assign_status <- function(id,
                          death_day,
                          last_day,
                          recovery_day,
                          tmax){

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
  return(c(id = id,
           time = time,
           status = status))
}

#' Generate the time to improvement dataset from an OTM dataset
#'
#' @param df An OTM dataset generated with otm::generate_dataset
#' @param death_state Which state corresponds to death? Must be integer.
#' @param threshold What is the threshold for improvement?
#' @param tmax The final day of the trial (used for censoring at death)
#' @return A survival dataset
#' @export
create_improvement <- function(df,
                            death_state,
                            threshold,
                            tmax){

  stopifnot("tmax must be an integer" = is.integer(tmax))
  stopifnot("death_state must be an integer" = is.integer(death_state))

  # Create a dataset that identifies for each subject
  # death day, last day, and recovery day
  surv_df <-
    df |>
    dplyr::mutate(y0 = yprev[[1]], .by = "id") |>
    dplyr::summarise(death_day = t[death_index(y,death_state = death_state)],
                     last_day = max(t),
                     recovery_day = t[improvement_index(y=y, y0=y0[[1]], threshold = threshold)],
                     .by="id")

  # For each subject, assign time and status based on
  # death day, last day, and recovery day
  # and format as a data.frame
  surv_df <-
  surv_df |>
    purrr::transpose() |>
    purrr::map_dfr(~assign_status(
                       id = .x$id,
                       death_day = .x$death_day,
                       last_day = .x$last_day,
                       recovery_day = .x$recovery_day,
                       tmax = tmax))

  id_df <-
    df |>
    dplyr::filter(t == 1) |>
    dplyr::select(id, tx)

  surv_df <- dplyr::inner_join(surv_df, id_df, by = "id")

  return(surv_df)
}

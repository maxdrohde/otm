# Declare variables for use in summarise
utils::globalVariables(c("y", "t"))

# Get the index that corresponds to death
# Return NA if not present
death_index <- function(y, death_state){
  index <- which(y == death_state)
  if (length(index) == 0L) return(NA_integer_)
  return(index)
}


# Get the first index that corresponds to recovery
# Return NA if not present
recovery_index <- function(y, recovery_states){
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

#' Generate the survival dataset from an OTM dataset
#'
#' @param df An OTM dataset generated with otm::generate_dataset
#' @param death_state Which state corresponds to death? Must be integer.
#' @param recovery_states Which state(s) correspond to recovery? Must be integer.
#' @param tmax The final day of the trial (used for censoring at death)
#' @return A survival dataset
#' @export
create_survival <- function(df,
                            death_state,
                            recovery_states,
                            tmax){

  stopifnot("tmax must be an integer" = is.integer(tmax))
  stopifnot("death_state must be an integer" = is.integer(death_state))
  stopifnot("recovery_states must be integers" = is.integer(recovery_states))

  # Create a dataset that identifies for each subject
  # death day, last day, and recovery day
  surv_df <-
    df |>
    dplyr::summarise(death_day = t[death_index(y,
                                                 death_state = death_state)],
                     last_day = max(t),
                     recovery_day = t[recovery_index(y,
                                                       recovery_states = recovery_states)],
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

  return(surv_df)
}

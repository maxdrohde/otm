get_surv_data_by_subj <- function(i, df) {

  # Filter to current subject
  id_df <- df[df$id == i, ]

  # Identify patient status
  recovered <- (1 %in% id_df$y) || (2 %in% id_df$y) || (3 %in% id_df$y)
  deceased <- (8 %in% id_df$y) && (!recovered)
  censored <- !(recovered | deceased)

  if (recovered) {
    # Recovery day is the first day at state 1,2, or 3
    # status = 1 is recovered / status = 0 is not recovered
    recovery_day <- id_df[id_df$y %in% 1:3, ]
    recovery_day <- min(recovery_day$day)
    return(list(id = i, time = recovery_day, status = 1))
  }

  if (deceased) {
    # Censor patients who died before recovery at day 29
    return(list(id = i, time = 29, status = 0))
  }

  if (censored) {
    # censor_day is the last recorded day
    censor_day <- max(id_df$day)
    return(list(id = i, time = censor_day, status = 0))
  }
}

#' Generate the survival dataset from an OTM dataset
#'
#' @param df An OTM dataset generated with otm::generate_dataset
#' @return A survival dataset
#' @export
create_survival <- function(df){
  surv_df <-
    purrr::map(unique(df$id),
               ~get_surv_data_by_subj(i = .x,
                                      df = df)) |>
    data.table::rbindlist()

  return(surv_df)
}

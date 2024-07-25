fit_free_days <-
  function(df,
           bad_states,
           death_state){

    df <- otm:::create_free_days(df = df,
                                 bad_states = bad_states,
                                 death_state = death_state)

    mod_full <- VGAM::vglm(formula = free_days ~ tx,
                           family = VGAM::propodds(),
                           data = df)

    mod_reduced <- VGAM::vglm(formula = free_days ~ 1,
                              family = VGAM::propodds(),
                              data = df)

    tx_p_value <- VGAM::lrtest(mod_full, mod_reduced)@Body[2,5]

    return(tx_p_value)
  }

safe_fit_free_days <- function(df,
                               bad_states,
                               death_state) {
  tryCatch({
    fit_free_days(df,
                  bad_states,
                  death_state)
  },
  error = function(e) {
    print("There was an error in the Free-Days model")
    print(e)
    return(NA)
  })
}

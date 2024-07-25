fit_single_day_ppo <-
  function(df,
           day,
           tmax,
           absorb){

  df <- otm:::carry_absorbing_forward(df,
                                      absorb = absorb,
                                      tmax = tmax)

  df <- dplyr::filter(df, t == day)

  mod_full <- VGAM::vglm(formula = y ~ tx,
                         family = VGAM::propodds(),
                         data = df)

  mod_reduced <- VGAM::vglm(formula = y ~ 1,
                            family = VGAM::propodds(),
                            data = df)

  tx_p_value <- VGAM::lrtest(mod_full, mod_reduced)@Body[2,5]

  return(tx_p_value)
  }

safe_fit_single_day_ppo <- function(df,
                         day,
                         tmax,
                         absorb) {
  tryCatch({
    fit_single_day_ppo(df, day, tmax, absorb)
  },
  error = function(e) {
    print("There was an error in the Single-Day PPO model")
    print(e)
    return(NA)
  })
}


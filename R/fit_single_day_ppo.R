# Fit a first-order OTM model with a specified number of knots
# and get the p-value for the treatment effect with a likelihood ratio test
fit_single_day_ppo <-
  function(df,
           day){

  df <- dplyr::filter(df, t == day)

  mod_full <- VGAM::vglm(formula = y ~ tx,
                         family = VGAM::cumulative(parallel = TRUE),
                         data = df)

  mod_reduced <- VGAM::vglm(formula = y ~ 1,
                            family = VGAM::cumulative(parallel = TRUE),
                            data = df)

  tx_p_value <- VGAM::lrtest(mod_full, mod_reduced)@Body[2,5]

  return(tx_p_value)
}


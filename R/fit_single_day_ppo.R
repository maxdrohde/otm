fit_single_day_ppo <-
  function(df,
           day,
           tmax,
           absorb,
           full_info){

  # Verify required columns
  df <- otm_verify(df)

  # Carry absorbing state forward
  df <- otm:::carry_absorbing_forward(df,
                                      absorb = absorb,
                                      tmax = tmax)

  # Filter to prespecified study day to analyze
  df <- dplyr::filter(df, t == day)

  # Fit PO model with treatment
  mod_full <- VGAM::vglm(formula = y ~ tx,
                         family = VGAM::propodds(),
                         data = df)

  # Fit PO model without treatment
  mod_reduced <- VGAM::vglm(formula = y ~ 1,
                            family = VGAM::propodds(),
                            data = df)

  # P-value from likelihood ratio test
  tx_p_value <- VGAM::lrtest(mod_full, mod_reduced)@Body[2,5]

  # If full_info, return the fitted models
  # Otherwise, just return the p-value
  if (full_info) {
    return(list(
      LR_pvalue  = tx_p_value,
      model = mod_full,
      reduced_model = mod_reduced
    ))
  }

  return(tx_p_value)
  }

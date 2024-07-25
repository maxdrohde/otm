# Fit a first-order OTM model with a specified number of knots
# and get the p-value for the treatment effect with a likelihood ratio test
fit_otm <- function(df,
                    knots){

  mod_full <- VGAM::vglm(formula = y ~ yprev + splines::ns(t, df = knots+ 1) * tx,
                    family = VGAM::propodds(),
                    data = df)

  mod_reduced <- VGAM::vglm(formula = y ~ yprev + splines::ns(t, df = knots+ 1),
                            family = VGAM::propodds(),
                            data = df)

  tx_p_value <- VGAM::lrtest(mod_full, mod_reduced)@Body[2,5]

  return(tx_p_value)
}

safe_fit_otm <- function(df,
                         knots) {
  tryCatch({
    fit_otm(df,
            knots)
  },
  error = function(e) {
    print("There was an error in the OTM model")
    print(e)
    return(NA)
  })
}

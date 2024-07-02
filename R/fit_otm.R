# Fit a first-order OTM model with a specified number of knots
# and get the p-value for the treatment effect with a likelihood ratio test
fit_otm <- function(df,
                    knots){

  mod_full <- VGAM::vglm(formula = y ~ yprev + splines::ns(t, df = knots+ 1) * tx,
                    family = VGAM::cumulative(parallel = TRUE),
                    data = df)

  mod_reduced <- VGAM::vglm(formula = y ~ yprev + splines::ns(t, df = knots+ 1),
                            family = VGAM::cumulative(parallel = TRUE),
                            data = df)

  tx_p_value <- VGAM::lrtest(mod_full, mod_reduced)@Body[2,5]

  return(tx_p_value)
}


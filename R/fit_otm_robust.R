# Fit a first-order OTM model with a specified number of knots
# and get the p-value for the treatment effect with Wald test using
# robust variance estimator if robust = TRUE
fit_otm_robust <- function(df, knots, robust) {

  mod <- rms::orm(
    y ~ yprev + splines::ns(t, df = knots + 1) * tx,
    y = TRUE,
    x = TRUE,
    data = df)

  if (robust) {
    mod <- rms::robcov(mod)
  }

  a <- anova(mod)
  tx_p_value <- a["tx  (Factor+Higher Order Factors)", "P"]

  return(tx_p_value)
}

safe_fit_otm_robust <- function(df,
                         knots,
                         robust) {
  tryCatch({
    fit_otm_robust(df, knots, robust)
  },
  error = function(e) {
    print("There was an error in the OTM-robust model")
    print(e)
    return(NA)
  })
}

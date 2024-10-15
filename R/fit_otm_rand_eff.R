fit_otm_rand_eff <- function(df, knots, rand_slope){

  if (rand_slope) {
    mod_full <-
      ordinal::clmm(formula = as.ordered(y) ~ yprev + splines::ns(t, df = knots+ 1) * tx + (1 + t|id),
                    nAGQ=1,
                    control = ordinal::clmm.control(maxIter = 1000000, method = "nlminb"),
                    link = "logit",
                    data = df)

    mod_reduced <-
      ordinal::clmm(formula = as.ordered(y) ~ yprev + splines::ns(t, df = knots+ 1) + (1 + t|id),
                    nAGQ=1,
                    control = ordinal::clmm.control(maxIter = 1000000, method = "nlminb"),
                    link = "logit",
                    data = df)
  } else{
    mod_full <-
      ordinal::clmm(formula = as.ordered(y) ~ yprev + splines::ns(t, df = knots+ 1) * tx + (1|id),
                    nAGQ=10,
                    control = ordinal::clmm.control(maxIter = 1000000, method = "nlminb"),
                    link = "logit",
                    data = df)

    mod_reduced <-
      ordinal::clmm(formula = as.ordered(y) ~ yprev + splines::ns(t, df = knots+ 1) + (1|id),
                    nAGQ=10,
                    control = ordinal::clmm.control(maxIter = 1000000, method = "nlminb"),
                    link = "logit",
                    data = df)
  }

  a <- ordinal:::anova.clm(mod_full, mod_reduced) |> as.data.frame()

  tx_p_value <- a$`Pr(>Chisq)`[[2]]

  return(tx_p_value)
}

safe_fit_otm_rand_eff <- function(df, knots, rand_slope) {
  tryCatch({
    fit_otm_rand_eff(df, knots, rand_slope)
  },
  error = function(e) {
    print("There was an error in the OTM random intecept model")
    print(e)
    return(NA)
  })
}

fit_rand_eff <- function(df, knots, rand_slope, diagnostics = FALSE, method = "nlminb"){

  MAXITER <- 5000

  if (rand_slope) {
    mod_full <-
      ordinal::clmm(formula = as.ordered(y) ~ splines::ns(t, df = knots+ 1) * tx + (1 + t|id),
                    nAGQ=1,
                    control = ordinal::clmm.control(maxIter = MAXITER, method = "nlminb"),
                    Hess = FALSE,
                    link = "logit",
                    data = df)

    mod_reduced <-
      ordinal::clmm(formula = as.ordered(y) ~ splines::ns(t, df = knots+ 1) + (1 + t|id),
                    nAGQ=1,
                    control = ordinal::clmm.control(maxIter = MAXITER, method = "nlminb"),
                    Hess = FALSE,
                    link = "logit",
                    data = df)
  } else{
    mod_full <-
      ordinal::clmm(formula = as.ordered(y) ~ splines::ns(t, df = knots+ 1) * tx + (1|id),
                    nAGQ=10,
                    control = ordinal::clmm.control(maxIter = MAXITER, method = "nlminb"),
                    Hess = FALSE,
                    link = "logit",
                    data = df)

    mod_reduced <-
      ordinal::clmm(formula = as.ordered(y) ~ splines::ns(t, df = knots+ 1) + (1|id),
                    nAGQ=10,
                    control = ordinal::clmm.control(maxIter = MAXITER, method = "nlminb"),
                    Hess = FALSE,
                    link = "logit",
                    data = df)
  }

  a <- ordinal:::anova.clm(mod_full, mod_reduced) |> as.data.frame()

  if (diagnostics) {
    return(
      list(
        "full" = mod_full,
        "reduced" = mod_reduced,
        "anova" = ordinal:::anova.clm(mod_full, mod_reduced)
      )
    )
  }

  tx_p_value <- a$`Pr(>Chisq)`[[2]]

  return(tx_p_value)
}

safe_fit_rand_eff <- function(df, knots, rand_slope) {
  tryCatch({
    fit_rand_eff(df, knots, rand_slope)
  },
  error = function(e) {
    print("There was an error in the random effects model")
    print(e)
    return(NA)
  })
}

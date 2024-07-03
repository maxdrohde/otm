fit_rand_eff <- function(df,
                    knots){

  df$y <- as.ordered(df$y)

  mod_full <- ordinal::clmm(formula = y ~ splines::ns(t, df = knots+ 1) * tx + (1|id),
                            link = "logit",
                            data = df)

  mod_reduced <- ordinal::clmm(formula = y ~ splines::ns(t, df = knots+ 1) + (1|id),
                               link = "logit",
                               data = df)

  tx_p_value <- as.data.frame(anova(mod_full, mod_reduced))$`Pr(>Chisq)`[[2]]

  return(tx_p_value)
}


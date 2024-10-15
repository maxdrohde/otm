fit_rand_eff <- function(df,
                    knots){

  df$y <- as.ordered(df$y)

  mod_full <- ordinal::clmm(formula = y ~ splines::ns(t, df = knots+ 1) * tx + (1|id),
                            link = "logit", control = ordinal::clmm.control(maxIter = 5000, maxLineIter = 5000),
                            data = df)

  mod_reduced <- ordinal::clmm(formula = y ~ splines::ns(t, df = knots+ 1) + (1|id),
                               link = "logit", control = ordinal::clmm.control(maxIter = 5000, maxLineIter = 5000),
                               data = df)

  res <- ordinal:::anova.clm(mod_full, mod_reduced)

  tx_p_value <- as.data.frame(res)$`Pr(>Chisq)`[[2]]

  return(tx_p_value)
}


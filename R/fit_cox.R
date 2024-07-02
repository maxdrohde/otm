# Fit Cox model and return the Wald p-value
fit_cox <- function(surv_df,
                    type,
                    knots) {
  if (type == "prop_hazard") {
    mod <-
      survival::coxph(survival::Surv(time, status) ~ tx,
        data = surv_df
      )

    tx_p_value <- summary(mod)$logtest[["pvalue"]]
  }

  if (type == "spline") {
    mod <-
      survival::coxph(survival::Surv(time, status) ~ tt(as.integer(tx)),
        tt = function(x, t, ...) x * splines::ns(t, df = knots + 1),
        data = surv_df
      )

    tx_p_value <- summary(mod)$logtest[["pvalue"]]
  }

  return(tx_p_value)
}

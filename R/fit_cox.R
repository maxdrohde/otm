fit_cox <- function(df,
                    recovery_states,
                    death_state,
                    tmax,
                    type,
                    knots) {

  stopifnot(type %in% c("prop_hazard", "spline"))

  surv_df <-
  otm:::create_survival(df,
                        tmax = tmax,
                        death_state = death_state,
                        recovery_states = recovery_states)


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

safe_fit_cox <- function(df,
                         recovery_states,
                         death_state,
                         tmax,
                         type,
                         knots) {
  tryCatch({
    fit_cox(df, recovery_states, death_state, tmax, type, knots)
  },
  error = function(e) {
    print("There was an error in the Cox model")
    print(e)
    return(NA)
  })
}

suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(otm))
suppressMessages(library(survival))
suppressMessages(library(glue))

# Set parameters
params <-
  list(
    cutpoints = c(
      2.704779695,
      3.283274970,
      3.759044255,
      6.152768017,
      9.928567811,
      12.594702347,
      19.080282386
    ),
    beta_yprev = c(
      0.006164718,
      3.668244235,
      4.756980385,
      7.696745715,
      11.098149358,
      15.471452261
    ),
    beta_time = -0.013360423,
    beta_regression = 0
  )

# Simulation function

run_sim <- function(n_subjects){

  tx_df <- generate_datasetC(params,
                            covariates = 1,
                            baseline_y = 5,
                            absorb = 8,
                            times = 1:28,
                            n_subjects = n_subjects/2) |>
    mutate(tx = 1)

  placebo_df <- generate_datasetC(params,
                                 covariates = 0,
                                 baseline_y = 5,
                                 absorb = 8,
                                 times = 1:28,
                                 n_subjects = n_subjects/2) |>
    mutate(tx = 0,
           id = id + length(unique(tx_df$id)))

  df <- rbind(tx_df,
              placebo_df) |>
    mutate(yprev = as.factor(yprev))

  surv_df <- create_survival(df, tmax=28L)

  demo <-
    df |>
    filter(day == 1) |>
    select(id, tx)

  surv_df <-
    left_join(surv_df, demo, by = "id")

  ## Fit models

  # Fit OTM
  mod <- ordinal::clm(as.ordered(y) ~ yprev + tx, data=df, maxIter=2000L)
  p_otm <- summary(mod)$coefficients["tx", "Pr(>|z|)"]

  # Fit Cox
  mod <- coxph(Surv(time, status) ~ tx,
               data = surv_df)

  p_cox <- summary(mod)$coefficients[1,5]

  return(data.frame(otm = p_otm,
                    cox = p_cox))
}

run_sim(2000)


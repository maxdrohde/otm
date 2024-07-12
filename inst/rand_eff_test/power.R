library(glue)

# Get the job array number
args <- commandArgs(trailingOnly=TRUE)
i <- as.integer(args[[1]])

# Set unique seed for this simulation
set.seed(i)

# Number of iterations to compute power
ITER <- 100

# 124
# PARAMETER SETTINGS FOR THE SIMULATION
GRID_sample_size <- seq(200, 500, 10)
GRID_cutpoints <- list(c(-4, -2, -1))
GRID_beta_t <- c(-0.1)
GRID_beta_tx <- c(0)
GRID_beta_t_tx <- c(0, -0.05, -0.2, -0.3)
tx_type <- "linear"

param_df <-
  tidyr::crossing(
    GRID_sample_size,
    GRID_cutpoints,
    GRID_beta_t,
    GRID_beta_tx,
    GRID_beta_t_tx)

row <- param_df[i,]

################# Job specific settings ########################################
n_subjects <- row$GRID_sample_size[[1]]
cutpoints <- row$GRID_cutpoints[[1]]
beta_t <- row$GRID_beta_t[[1]]
beta_tx <- row$GRID_beta_tx[[1]]
beta_t_tx <- row$GRID_beta_t_tx[[1]]

################# Simulation code ##############################################
run_sim <- function(){

  times <- 1L:10L
  tmax <- max(times)
  death_state <- 8L
  recovery_states <- c(1L)


df <-
otm:::generate_ordinal_random_effects_data(
  N = n_subjects,
  cutpoints = cutpoints,
  beta_t = beta_t,
  beta_tx = beta_tx,
  beta_t_tx = beta_t_tx,
  times = times,
  rand_intercept_sd = 4.51,
  rand_slope_sd = 0.45,
  rand_eff_corr = 0)


  # Create the survival data frame
  surv_df <- otm:::create_survival(df,
                             tmax = tmax,
                             death_state = death_state,
                             recovery_states = recovery_states)

  # Merge in tx information
  surv_df <-
    df |>
    dplyr::filter(t == 1) |>
    dplyr::select(id, tx) |>
    dplyr::right_join(surv_df, by = "id")

  #### FIT MODELS ####
  res <-
    data.frame(
      otm_spline_3 = otm:::fit_otm(df, knots = 2),
      rand_eff_spline_3 = otm:::fit_rand_eff(df, knots = 2),
      cox_prop = otm:::fit_cox(surv_df, type = "prop_hazard", knots = "not applicable"),
      cox_spline_3 = otm:::fit_cox(surv_df, type = "spline", knots = 2)
    )

  return(res)
}

# Run simulation
res <-
  purrr::map(1:ITER,
             ~ run_sim(),
             .progress = TRUE) |>
  purrr::list_rbind()

res <-
  res |>
  tidyr::pivot_longer(everything(),
               names_to = "model",
               values_to = "p_value")

res <-
res |>
  dplyr::summarise(power = mean(p_value < 0.05, na.rm = TRUE),
            prop_na = mean(is.na(p_value)),
            .by = "model")

# ADD METADATA
res$iterations <- ITER
res$sample_size <- n_subjects
res$beta_t <- beta_t
res$beta_tx <- beta_tx
res$beta_t_tx <- beta_t_tx
res$tx_type <- tx_type

# Write out results to correct folder as CSV
readr::write_csv(res, glue("results/res_{i}.csv"))

# WARNINGS
print("WARNINGS:")
print(warnings())

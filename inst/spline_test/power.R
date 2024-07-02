library(glue)

# Get the job array number
args <- commandArgs(trailingOnly=TRUE)
i <- as.integer(args[[1]])

# Set unique seed for this simulation
set.seed(i)

# Number of iterations to compute power
ITER <- 1000

# 324
# PARAMETER SETTINGS FOR THE SIMULATION
GRID_sample_size <- c(300)
GRID_baseline_y <- c(4L, 5L, 6L, 7L)
GRID_cutpoints <- list(c(2.7, 3.3, 3.8, 6.2, 9.9, 12.6, 19.1))
GRID_beta_yprev <- list(c(0.006, 3.7, 4.8, 7.7, 11.1, 15.5))
GRID_beta_t <- c(-0.02)
GRID_beta_tx <- c(0)
GRID_beta_t_tx <- c(0, -0.01, -0.02)
GRID_tx_end <- seq(2, 28, 1)
tx_type <- "linear"

param_df <-
  tidyr::crossing(
    GRID_sample_size,
    GRID_baseline_y,
    GRID_cutpoints,
    GRID_beta_yprev,
    GRID_beta_t,
    GRID_beta_tx,
    GRID_beta_t_tx,
    GRID_tx_end)

row <- param_df[i,]

################# Job specific settings ########################################
n_subjects <- row$GRID_sample_size[[1]]
cutpoints <- row$GRID_cutpoints[[1]]
beta_yprev <- row$GRID_beta_yprev[[1]]
beta_t <- row$GRID_beta_t[[1]]
beta_tx <- row$GRID_beta_tx[[1]]
beta_t_tx <- row$GRID_beta_t_tx[[1]]
baseline_y <- row$GRID_baseline_y[[1]]
tx_end <- row$GRID_tx_end[[1]]

################# Simulation code ##############################################
run_sim <- function(){

  times <- 1L:28L
  tmax <- max(times)
  death_state <- 8L
  recovery_states <- c(1L, 2L, 3L)

  # Generate OTM dataset
  df <- otm:::generate_dataset(cutpoints = cutpoints,
                         beta_yprev = beta_yprev,
                         beta_t = beta_t,
                         beta_tx = beta_tx,
                         beta_t_tx = beta_t_tx,
                         tx_end = tx_end,
                         baseline_y = baseline_y,
                         times = times,
                         tx_type = tx_type,
                         absorb = 8,
                         n_subjects = n_subjects)


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
      otm_spline_3 = otm:::fit_otm(df, knots = 3),
      otm_spline_6 = otm:::fit_otm(df, knots = 6),
      cox_prop = otm:::fit_cox(surv_df, type = "prop_hazard", knots = "not applicable"),
      cox_spline_3 = otm:::fit_cox(surv_df, type = "spline", knots = 3),
      cox_spline_4 = otm:::fit_cox(surv_df, type = "spline", knots = 4)
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
res$baseline_y <- baseline_y
res$beta_t <- beta_t
res$beta_tx <- beta_tx
res$beta_t_tx <- beta_t_tx
res$tx_end <- tx_end
res$tx_type <- tx_type

# Write out results to correct folder as CSV
readr::write_csv(res, glue("results/res_{i}.csv"))

# WARNINGS
print("WARNINGS:")
print(warnings())

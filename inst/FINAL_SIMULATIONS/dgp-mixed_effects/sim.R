#-------------------------------------------------------------------------------
# Read in the arguments from the command line
args <- commandArgs(trailingOnly=TRUE)
i <- as.integer(args[[1]])

# Set the seed based on the current job number
set.seed(i)
#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
ITER <- 500
#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
# Global parameters
times <- 1L:28L
tmax <- max(times)
death_state <- 999L
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# 80
# Define simulation settings
sim_settings <-
  expand.grid(
    sample_size = c(100, 200, 300, 400, 500),
    cutpoints = list(c(-5, -3, -2, 0, 1, 2, 4)),
    beta_t = c(-0.2),
    beta_tx = c(0, -0.2),
    beta_t_tx = c(0, -0.01),
    rand_intercept_sd = c(0.00001, 1),
    rand_slope_sd = c(0.00001, 0.05)
  )

cat(glue::glue("--------{nrow(sim_settings)} simulation settings--------\n\n"))

params <- sim_settings[i,]
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
run_sim <- function() {
  # NOTE: The arguments of this function are set after the definition of the
  # function body

  # Save arguments to append as metadata
  arguments <- as.list(environment())

  # Print parameters of this simulation for monitoring
  cat(glue::glue("Running simulation setting {i} with {n_sim} iterations"))
  cat("\n------------------------------------------\n")
  cat("SIMULATION PARAMETERS\n")
  for (cn in names(params)) {
    print(glue::glue("{cn}: {params[[cn]]}"))
  }
  cat("------------------------------------------\n")
  cat("\n")

  # Storage for each data frame of results
  data_frames <- vector("list", length = n_sim)

  iter_times <- rep(NA_real_, n_sim)
  for (SIM in 1:n_sim) {
    start <- bench::hires_time()

    # Generate dataset
    df <-
      otm:::generate_ordinal_random_effects_data(
        cutpoints = cutpoints[[1]],
        beta_t = beta_t[[1]],
        beta_tx = beta_tx[[1]],
        beta_t_tx = beta_t_tx[[1]],
        times = times,
        rand_intercept_sd = rand_intercept_sd[[1]],
        rand_slope_sd = rand_slope_sd[[1]],
        rand_eff_corr = 0,
        n_subjects = sample_size[[1]]
      )

    # Fit models to the data and store p-values
    res <-
      data.frame(
        otm = otm:::safe_fit_otm(df, knots = 0),
        otm_rand_int = otm:::safe_fit_otm_rand_eff(df, knots = 0, rand_slope = FALSE),
        rand_int = otm:::safe_fit_rand_eff(df, knots = 0, rand_slope = FALSE),
        free_days8 = otm:::safe_fit_free_days(df, bad_states = c(8L), death_state = death_state),
        free_days78 = otm:::safe_fit_free_days(df, bad_states = c(7L, 8L), death_state = death_state),
        free_days678 = otm:::safe_fit_free_days(df, bad_states = c(6L, 7L, 8L), death_state = death_state),
        free_days5678 = otm:::safe_fit_free_days(df, bad_states = c(5L, 6L, 7L, 8L), death_state = death_state),
        free_days45678 = otm:::safe_fit_free_days(df, bad_states = c(4L, 5L, 6L, 7L, 8L), death_state = death_state),
        free_days345678 = otm:::safe_fit_free_days(df, bad_states = c(3L, 4L, 5L, 6L, 7L, 8L), death_state = death_state),
        free_days2345678 = otm:::safe_fit_free_days(df, bad_states = c(2L, 3L, 4L, 5L, 6L, 7L, 8L), death_state = death_state),
        ppo_3 = otm:::safe_fit_single_day_ppo(df, 3, absorb = death_state, tmax = tmax),
        ppo_7 = otm:::safe_fit_single_day_ppo(df, 7, absorb = death_state, tmax = tmax),
        ppo_14 = otm:::safe_fit_single_day_ppo(df, 14, absorb = death_state, tmax = tmax),
        ppo_21 = otm:::safe_fit_single_day_ppo(df, 21, absorb = death_state, tmax = tmax),
        ppo_28 = otm:::safe_fit_single_day_ppo(df, 28, absorb = death_state, tmax = tmax),
        cox_prop_state1 = otm:::safe_fit_cox(df, recovery_states = c(1L), death_state = death_state, tmax = tmax, type = "prop_hazard", knots = "not applicable"),
        cox_spline2_state1 = otm:::safe_fit_cox(df, recovery_states = c(1L), death_state = death_state, tmax = tmax, type = "spline", knots = 2L),
        cox_prop_state12 = otm:::safe_fit_cox(df, recovery_states = c(1L, 2L), death_state = death_state, tmax = tmax, type = "prop_hazard", knots = "not applicable"),
        cox_spline2_state12 = otm:::safe_fit_cox(df, recovery_states = c(1L, 2L), death_state = death_state, tmax = tmax, type = "spline", knots = 2L),
        cox_prop_state123 = otm:::safe_fit_cox(df, recovery_states = c(1L, 2L, 3L), death_state = death_state, tmax = tmax, type = "prop_hazard", knots = "not applicable"),
        cox_spline2_state123 = otm:::safe_fit_cox(df, recovery_states = c(1L, 2L, 3L), death_state = death_state, tmax = tmax, type = "spline", knots = 2L)
        )

        data_frames[SIM] <- list(res)

        end <-  bench::hires_time()
        elapsed <- end - start
        iter_times[[SIM]] <- elapsed
        s <- summary(iter_times, na.rm = TRUE)
        std <- sd(iter_times, na.rm = TRUE)
        middle <- ((s[[4]]) *(n_sim - SIM)) |> bench::as_bench_time()
        low <- ((s[[2]]) *(n_sim - SIM)) |> bench::as_bench_time()
        high <- ((s[[5]]) *(n_sim - SIM)) |> bench::as_bench_time()
        print(glue::glue("(Iteration: {SIM}) ({((SIM / n_sim) * 100) |> round(2)}%)    Estimated time left: {middle} [{low}, {high}]"))
  }

  # Merge all simulation data frames
  power <- purrr::list_rbind(data_frames)

  # Pivot from wide to long
  power <-
    power |>
    tidyr::pivot_longer(everything(),
                        names_to = "model",
                        values_to = "p_value")

  # Compute power based on p < 0.05
  # and compute the proportion of failed simulations
  power <-
    power |>
    dplyr::summarise(power = mean(p_value < 0.05,
                                  na.rm = TRUE),
                     prop_na = mean(is.na(p_value)),
                     .by = "model")

  # Append the simulation parameters and the datetime
  power <- data.table::data.table(power, data.table::as.data.table(arguments))
  power$datetime <- Sys.time()
  datetime <- as.integer(Sys.time())

  # Write the results out to CSV
  filename <- glue::glue("{i}_{datetime}.csv")
  readr::write_csv(x = power, file = glue::glue("results/{filename}"))
  print(glue::glue("CSV file written for simulation {i}"))
}

# Set the arguments of run_sim() based on the
# variables declared in the simulation setting
function_arguments <- c(names(params), "n_sim")
formals(run_sim) <- setNames(vector("list", length(function_arguments)), function_arguments)
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Run the simulation with the specified parameters
do.call(run_sim, c(params, n_sim = ITER))

print(warnings())
#-------------------------------------------------------------------------------

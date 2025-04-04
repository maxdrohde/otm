#-------------------------------------------------------------------------------
# Read in the arguments from the command line
args <- commandArgs(trailingOnly=TRUE)

# First argument is the scenario
i <- as.integer(args[[1]])

# Second argument is the iteration
j <- as.integer(args[[2]])

# Create folder for the results
folder <- glue::glue("results/{i}")
if (!dir.exists(folder)) {
  dir.create(folder)
  print(paste("Folder created at:", folder))
} else {
  print("The folder already exists.")
}

# Set the seed based on the current job number
set.seed(j)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
ITER <- 5
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Global parameters
times <- 1L:28L
tmax <- max(times)
death_state <- 8L
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# 25
# Define simulation settings
sim_settings <-
  expand.grid(
    sample_size = c(100, 200, 300, 400, 500),
    cutpoints = list(c(2.7, 3.3, 3.8, 6.2, 9.9, 12.6, 19.1)),
    beta_yprev = list(c(0, 3.7, 4.8, 7.7, 11.1, 15.5)),
    beta_t = c(-0.02),
    beta_tx = c(0, -0.1),
    beta_t_tx = c(0),
    tx_end = c(3, 7, 14, 28),
    tx_type = "constant"
  )

# Remove redundant settings
sim_settings <-
  sim_settings |>
  dplyr::filter(!((beta_tx == 0) & (tx_end != 28)))

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

    baseline_y <-
      list(states = c(4,5,6,7),
           proportions = c(0.25, 0.25, 0.25, 0.25))

    # Generate dataset
     df <-
      otm:::generate_otm_data(
        cutpoints = cutpoints[[1]],
        beta_yprev = beta_yprev[[1]],
        beta_t = beta_t[[1]],
        beta_tx = beta_tx[[1]],
        beta_t_tx = beta_t_tx[[1]],
        tx_end = tx_end[[1]],
        tx_type = tx_type[[1]],
        n_subjects = sample_size[[1]],
        baseline_y = baseline_y,
        times = times,
        absorb = death_state
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
        cox_prop_state1_imp = otm:::safe_fit_cox_improvement(df, threshold = 1L, death_state = death_state, tmax = tmax, type = "prop_hazard", knots = "not applicable"),
        cox_spline2_state1_imp = otm:::safe_fit_cox_improvement(df, threshold = 1L, death_state = death_state, tmax = tmax, type = "spline", knots = 2L),
        cox_prop_state2_imp = otm:::safe_fit_cox_improvement(df, threshold = 2L, death_state = death_state, tmax = tmax, type = "prop_hazard", knots = "not applicable"),
        cox_spline2_state2_imp = otm:::safe_fit_cox_improvement(df, threshold = 2L, death_state = death_state, tmax = tmax, type = "spline", knots = 2L),
        cox_prop_state3_imp = otm:::safe_fit_cox_improvement(df, threshold = 3L, death_state = death_state, tmax = tmax, type = "prop_hazard", knots = "not applicable"),
        cox_spline2_state3_imp = otm:::safe_fit_cox_improvement(df, threshold = 3L, death_state = death_state, tmax = tmax, type = "spline", knots = 2L),
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

  # Append the simulation parameters and the datetime
  power <- data.table::data.table(power, data.table::as.data.table(arguments))
  power$datetime <- Sys.time()
  datetime <- as.integer(Sys.time())

  power$setting <- i
  power$iteration <- j

  # Write the results out to CSV
  filename <- glue::glue("{i}_{j}_{datetime}.csv")
  readr::write_csv(x = power, file = glue::glue("results/{i}/{filename}"))

  print(glue::glue("CSV file: {filename}"))
  print(glue::glue("CSV file written for simulation {i} iteration {j} with a chunk size of {ITER}"))
  print("-----------------------------------------------------------------------------------")
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

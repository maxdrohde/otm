library(tidyverse)

# Get paths of all the CSV files
paths <- fs::dir_ls(path = "./results",
                    glob = "*.csv")

# Read in all CSV files
# Attach a unique ID to each simulation setting
# Pivot data longer
df <-
  map(paths,
      ~read_csv(.x, show_col_types = FALSE),
      .progress = TRUE) |>
  list_rbind() |>
  rowid_to_column("sim_id") |>
  select(-starts_with("n_na")) |>
  pivot_longer(cols = ends_with("power"),
               names_to = "model", 
               values_to = "power")

# df$model <-
#   case_match(
#     df$model,
#     "otm_power" ~ "Ordinal Transition Model",
#     "cox_power" ~ "Cox Proportional Hazards Model",
#     "po7_power" ~ "Proportional Odds Model (Day 7)",
#     "po14_power" ~ "Proportional Odds Model (Day 14)",
#     "po21_power" ~ "Proportional Odds Model (Day 21)",
#     "po28_power" ~ "Proportional Odds Model (Day 28)"
#   )

# Write out results to home folder
write_csv(df,
          file = "/home/rohdemd/simulation_results.csv")


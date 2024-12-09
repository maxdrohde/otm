library(tidyverse)

# Get paths for all CSV files in the results folder
paths <- fs::dir_ls(path = "./results",
                    glob = "*.csv",
                    recurse = TRUE)

# Merge all CSV files into a data table
df <-
  map(paths,
      ~data.table::fread(.x),
      .progress = TRUE) |>
  data.table::rbindlist()

# Pivot from wide to long
df <-
  df |>
  tidyr::pivot_longer(otm:cox_spline2_state123,
                      names_to = "model",
                      values_to = "p_value")

# Compute power based on p < 0.05
# and compute the proportion of failed simulations
power <-
  df |>
  dplyr::summarise(power = mean(p_value < 0.05,
                                na.rm = TRUE),
                   iterations = n(),
                   prop_na = mean(is.na(p_value)),
                   .by = c("model", "setting"))

metadata <-
  df |>
  select(sample_size:iteration) |>
  select(-datetime, -iteration) |>
  distinct(setting, .keep_all = TRUE)

res <- dplyr::left_join(power, metadata, by = "setting")

# Write merged data table to CSV
write_csv(res, file = "simulation_results.csv")


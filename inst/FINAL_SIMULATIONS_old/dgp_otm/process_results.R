library(tidyverse)

# Get paths for all CSV files in the results folder
paths <- fs::dir_ls(path = "./results",
                    glob = "*.csv")

# Merge all CSV files into a data table
df <-
  map(paths,
      ~data.table::fread(.x),
      .progress = TRUE) |>
  data.table::rbindlist()

# Write merged data table to CSV
write_csv(df, file = "simulation_results.csv")


source("/Users/max/otm/inst/benchmarking/power_null.R")

profvis::profvis(run_sim(2000))

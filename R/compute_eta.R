compute_eta <- function(beta_yprev,
                        beta_t,
                        beta_tx,
                        beta_t_tx,
                        tx_type,
                        tx_end,
                        yprev,
                        t,
                        tx) {
  # Use if treatment effect is constant over time
  if (tx_type == "constant") {
    eta <-
      ifelse(yprev == 1, 0, beta_yprev[[yprev - 1]]) +
      beta_tx * tx * ifelse(t <= tx_end, 1, 0) +
      beta_t * t
  }

  # Use if treatment effect changes linearly over time
  if (tx_type == "linear") {
    eta <-
      ifelse(yprev == 1, 0, beta_yprev[[yprev - 1]]) +
      beta_tx * tx * ifelse(t <= tx_end, 1, 0) +
      beta_t * t +
      beta_t_tx * tx * ifelse(t > tx_end, tx_end + 1, t)
  }

  return(eta)
}

params <-
  list(cutpoints = c(2.704779695, 3.283274970, 3.759044255, 6.152768017, 9.928567811, 12.594702347, 19.080282386),
       beta_yprev = -c(-0.006164718,  -3.668244235, -4.756980385, -7.696745715, -11.098149358, -15.471452261),
       beta_time = -0.013360423,
       beta_regression = -0.094833284)

compute_transition_probabilities(params = params,
                                 yprev = 4,
                                 day = 14,
                                 covariates = 1) |> round(3)


generate_record(params, 1, 4, 1:28, 8)


# Verify that simulating data gives same results as theoretical SOP calculation

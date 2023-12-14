#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

double expit(double x) {
  return exp(x) / (1 + exp(x));
}

//' @export
// [[Rcpp::export]]
NumericVector compute_transition_probabilitiesC(List params, int yprev, int day, NumericVector covariates
) {

  double yprev_effect = 0;
  double eta = 0;

  // Convert elements of params list
  NumericVector cutpoints = as<NumericVector>(params["cutpoints"]);
  NumericVector beta_yprev = as<NumericVector>(params["beta_yprev"]);
  double beta_time = as<double>(params["beta_time"]);
  NumericVector beta_regression = as<NumericVector>(params["beta_regression"]);

  // Set the effect of previous state
  if (yprev == 1) {
    yprev_effect = 0;
  } else{
    yprev_effect = beta_yprev[yprev - 2];
  }

  // Compute linear predictor
  eta =
  std::inner_product(beta_regression.begin(),
                     beta_regression.end(),
                     covariates.begin(), 0.0) +
        (beta_time * day) +
        (yprev_effect);

    // Compute cumulative probabilities
    NumericVector probs;
    int n = cutpoints.size();

    for(int i = 0; i < n; ++i) {
      probs.push_back(expit(cutpoints[i] - eta));
    }

    // Compute probabilities through differences in cumulative probs
    NumericVector res(n + 1);

    // Handle the first element separately
    res[0] = probs[0];

    // Loop starts from 1 as the first element is already handled
    for (int i = 1; i < n; ++i) {
        res[i] = probs[i] - probs[i-1];
    }

    // Handle the last element separately
    res[n] = 1.0 - probs[n-1];

  return(res);
}

//' @export
 // [[Rcpp::export]]
DataFrame generate_datasetC(List params, NumericVector covariates, int baseline_y, IntegerVector times, int absorb, int n_subjects) {

  int y;

  // Convert elements of params list
  NumericVector cutpoints = as<NumericVector>(params["cutpoints"]);

  IntegerVector y_levels = seq(1, cutpoints.size() + 1);

  // Store the simulated values
  std::vector<int> day_stored;
  std::vector<int> y_stored;
  std::vector<int> yprev_stored;
  std::vector<int> id_stored;

  // Reserve space for the vectors
  int estimated_size = n_subjects * times.size();
  day_stored.reserve(estimated_size);
  y_stored.reserve(estimated_size);
  yprev_stored.reserve(estimated_size);
  id_stored.reserve(estimated_size);

  // Loop over IDs
  for (int id = 1; id < n_subjects + 1; ++id)
  {
            // Counter for the number of iterations
            int i = 0;

            // Set day to the first time
            int day = times[0];

            // Baseline state
            int yprev = baseline_y;

            while (TRUE) {

              // Calculate the transition probabilities from the fitted model
              NumericVector transition_probabilities = compute_transition_probabilitiesC(params, yprev, day, covariates);

              // Move to the next state according to the transition probabilities
              y  = sample(y_levels, 1, 0, transition_probabilities)[0];

              // Save the values
              day_stored.push_back(day);
              y_stored.push_back(y);
              yprev_stored.push_back(yprev);
              id_stored.push_back(id);

              // Increment counter
              i = i + 1;

              // Break condition
              if ((i > times.size() - 1) || (y == absorb)) {
                break;
              }

              // Set next day
              day = times[i];
              yprev = y;
            }
  }
  
  DataFrame df = DataFrame::create(Named("day") = day_stored, Named("y") = y_stored, Named("yprev") = yprev_stored, Named("id") = id_stored);

  return(df);
}

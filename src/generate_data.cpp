#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// Define the expit function in C++ for speed
double expit(double x) {
  return exp(x) / (1 + exp(x));
}

//' Compute a vector of transition probability from an OTM
//'
//' @description
//' Given the parameters from an ordinal transition model (OTM),
//' calculate the transition probabilities for a given time and previous state.
//'
//' @param params A named list containing the parameters of the OTM.
//' Must include "beta_yprev": k-1 parameters for the k previous states,
//' "beta_regression": parameters for each covariates,
//' "beta_time": a single slope for the time parameter, and
//' "cutpoints": the cutpoint parameters.
//' @param yprev The previous state to be conditioned on.
//' @param day The day to compute the transition probabilities for.
//' @param covariates A vector of covariates
//' @return A vector of transition probabilities.
//' @export
// [[Rcpp::export]]
NumericVector compute_transition_probabilities(List params,
                                                int yprev,
                                                int day,
                                                NumericVector covariates)
{
  double yprev_effect = 0;
  double eta = 0;

  // Convert elements of params list into C++ objects
  NumericVector cutpoints = as<NumericVector>(params["cutpoints"]);
  NumericVector beta_yprev = as<NumericVector>(params["beta_yprev"]);
  double beta_time = as<double>(params["beta_time"]);
  NumericVector beta_regression = as<NumericVector>(params["beta_regression"]);

  // Set the effect of previous state
  // Remember that C++ uses 0-based indexing
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

  // Compute probabilities through differences in cumulative probabilities
  NumericVector res(n + 1);
  // First element
  res[0] = probs[0];
  for (int i = 1; i < n; ++i) {
      res[i] = probs[i] - probs[i-1];
  }
  // Last Element
  res[n] = 1.0 - probs[n-1];

  return(res);
}

//' Generate a dataset of OTM data (with C++)
//'
//' @param params A named list containing the parameters of the OTM.
//' Must include "beta_yprev": k-1 parameters for the k previous states,
//' "beta_regression": parameters for each covariates,
//' "beta_time": a single slope for the time parameter, and
//' "cutpoints": the cutpoint parameters.
//' @param covariates A vector of covariates
//' @param baseline_y The baseline state of the subjects used when generating the data
//' @param times A vector of times to generate data for
//' @param absorb Is the last state absorbing?
//' @param n_subjects How many subject records should be generated?
//' @return A data frame of data generated under the first order OTM model.
//' @export
 // [[Rcpp::export]]
DataFrame generate_dataset(List params,
                            NumericVector covariates,
                            int baseline_y,
                            IntegerVector times,
                            bool absorb,
                            int n_subjects)
{

  int y;
  int absorbing_state;

  // Get cutpoints from the params list
  NumericVector cutpoints = as<NumericVector>(params["cutpoints"]);

  IntegerVector y_levels = seq(1, cutpoints.size() + 1);

  if (absorb == TRUE){
    // Set absorbing state to last state
    absorbing_state = cutpoints.size() + 1;
  } else{
    // Set to a sentinel value
    absorbing_state = 9999;
  }

  // Store the simulated values
  // Use C++ std::vector<int> rather than IntegerVector
  // because we are able to grow them efficiently unlike in R
  std::vector<int> day_stored;
  std::vector<int> y_stored;
  std::vector<int> yprev_stored;
  std::vector<int> id_stored;

  // Reserve space for the vectors (important for speed!)
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
              NumericVector transition_probabilities = compute_transition_probabilities(params, yprev, day, covariates);

              // Move to the next state according to the transition probabilities
              y  = sample(y_levels, 1, 0, transition_probabilities)[0];

              // Save the values
              day_stored.push_back(day);
              y_stored.push_back(y);
              yprev_stored.push_back(yprev);
              id_stored.push_back(id);

              // Increment counter
              i = i + 1;

              // Break condition if reach end of trial
              if (i > times.size() - 1) {
                break;
              }

              // Break if hit absorbing state
              if ((absorb == TRUE) && (y == absorbing_state)) {
                break;
              }

              // Set next day
              day = times[i];
              yprev = y;
      }
  }

  // Create a data.frame from the results of the data simulation
  DataFrame df = DataFrame::create(Named("day") = day_stored,
                                   Named("y") = y_stored,
                                   Named("yprev") = yprev_stored,
                                   Named("id") = id_stored);

  return(df);
}

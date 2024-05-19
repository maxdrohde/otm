#include <Rcpp.h>
#include <vector>
#include "compute_transition_probabilities.h"

using namespace Rcpp;

//' Generate a dataset of OTM data (with C++)
//'
//' @param params A named list containing the parameters of the OTM.
//' Must include "beta_yprev": k-1 parameters for the k previous states,
//' "beta_treatment": parameter for the treatment effect,
//' "beta_time": a single slope for the time parameter, and
//' "cutpoints": the cutpoint parameters.
//' @param treatment Treatment group? 0 or 1.
//' @param baseline_y The baseline state of the subjects used when generating the data
//' @param times A vector of times to generate data for
//' @param absorb Is the last state absorbing?
//' @param n_subjects How many subject records should be generated?
//' @return A data frame of data generated under the first order OTM model.
//' @export
// [[Rcpp::export]]
DataFrame generate_dataset(List params,
                           int treatment,
                           int baseline_y,
                           IntegerVector times,
                           bool absorb,
                           int n_subjects)
{

  int y;
  int absorbing_state;

  // Get cutpoints from the params list
  NumericVector cutpoints = params["cutpoints"];

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
              NumericVector transition_probabilities = compute_transition_probabilities(params, yprev, day, treatment);

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

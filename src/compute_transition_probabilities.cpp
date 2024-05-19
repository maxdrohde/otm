#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

//-----------------------------------------------------------------------------
// Define the expit function in C++ for speed
inline double expit(double x) {
  return 1.0 / (1.0 + exp(-x));
}
//-----------------------------------------------------------------------------


//' Compute a vector of transition probability from an OTM
//'
//' @description
//' Given the parameters from an ordinal transition model (OTM),
//' calculate the transition probabilities for a given time and previous state.
//'
//' @param params A named list containing the parameters of the OTM.
//' Must include "beta_yprev": k-1 parameters for the k previous states,
//' "beta_treatment": parameter for the treatment effect,
//' "beta_time": a single slope for the time parameter, and
//' "cutpoints": the cutpoint parameters.
//' @param yprev The previous state to be conditioned on.
//' @param day The day to compute the transition probabilities for.
//' @param treatment Treatment group? 0 or 1.
//' @return A vector of transition probabilities.
//' @export
// [[Rcpp::export]]
 NumericVector compute_transition_probabilities(List params,
                                                int yprev,
                                                int day,
                                                int treatment)
 {

  // Convert elements of params list into C++ objects
  const NumericVector cutpoints = params["cutpoints"];
  const NumericVector beta_yprev = params["beta_yprev"];
  const double beta_time = params["beta_time"];
  const double beta_treatment = params["beta_treatment"];

  // Number of cutpoints
  const int n = cutpoints.size();

  // Effect of previous state
  const double yprev_effect = (yprev == 1) ? 0 : beta_yprev[yprev - 2];

  // Linear Predictor
  const double eta = (beta_time * day) + yprev_effect + (beta_treatment * treatment);

  // Compute cumulative probabilities
  NumericVector probs(n);
  for(int i = 0; i < n; ++i) {
    probs[i] = expit(cutpoints[i] - eta);
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

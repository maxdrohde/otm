#ifndef OTM_FUNCTIONS_H
#define OTM_FUNCTIONS_H

#include <Rcpp.h>

// Declare the expit function
double expit(double x);

// Declare the compute_transition_probabilities function
Rcpp::NumericVector compute_transition_probabilities(Rcpp::List params,
                                                     int yprev,
                                                     int day,
                                                     int treatment);

#endif // OTM_FUNCTIONS_H

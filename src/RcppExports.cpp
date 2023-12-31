// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// compute_transition_probabilities
NumericVector compute_transition_probabilities(List params, int yprev, int day, NumericVector covariates);
RcppExport SEXP _otm_compute_transition_probabilities(SEXP paramsSEXP, SEXP yprevSEXP, SEXP daySEXP, SEXP covariatesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< int >::type yprev(yprevSEXP);
    Rcpp::traits::input_parameter< int >::type day(daySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type covariates(covariatesSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_transition_probabilities(params, yprev, day, covariates));
    return rcpp_result_gen;
END_RCPP
}
// generate_dataset
DataFrame generate_dataset(List params, NumericVector covariates, int baseline_y, IntegerVector times, bool absorb, int n_subjects);
RcppExport SEXP _otm_generate_dataset(SEXP paramsSEXP, SEXP covariatesSEXP, SEXP baseline_ySEXP, SEXP timesSEXP, SEXP absorbSEXP, SEXP n_subjectsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type covariates(covariatesSEXP);
    Rcpp::traits::input_parameter< int >::type baseline_y(baseline_ySEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type times(timesSEXP);
    Rcpp::traits::input_parameter< bool >::type absorb(absorbSEXP);
    Rcpp::traits::input_parameter< int >::type n_subjects(n_subjectsSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_dataset(params, covariates, baseline_y, times, absorb, n_subjects));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_otm_compute_transition_probabilities", (DL_FUNC) &_otm_compute_transition_probabilities, 4},
    {"_otm_generate_dataset", (DL_FUNC) &_otm_generate_dataset, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_otm(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rho_value
int rho_value(double x, int row, int col);
RcppExport SEXP _carsimr_geisler_rho_value(SEXP xSEXP, SEXP rowSEXP, SEXP colSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type row(rowSEXP);
    Rcpp::traits::input_parameter< int >::type col(colSEXP);
    rcpp_result_gen = Rcpp::wrap(rho_value(x, row, col));
    return rcpp_result_gen;
END_RCPP
}
// randomShuffle
NumericVector randomShuffle(NumericVector a);
RcppExport SEXP _carsimr_geisler_randomShuffle(SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP);
    rcpp_result_gen = Rcpp::wrap(randomShuffle(a));
    return rcpp_result_gen;
END_RCPP
}
// generate_and_shuffle
NumericVector generate_and_shuffle(int n, int n2, double p);
RcppExport SEXP _carsimr_geisler_generate_and_shuffle(SEXP nSEXP, SEXP n2SEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type n2(n2SEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_and_shuffle(n, n2, p));
    return rcpp_result_gen;
END_RCPP
}
// initialize_grid_cpp
NumericMatrix initialize_grid_cpp(double rho, int r, int c, double p, int seed);
RcppExport SEXP _carsimr_geisler_initialize_grid_cpp(SEXP rhoSEXP, SEXP rSEXP, SEXP cSEXP, SEXP pSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< int >::type r(rSEXP);
    Rcpp::traits::input_parameter< int >::type c(cSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(initialize_grid_cpp(rho, r, c, p, seed));
    return rcpp_result_gen;
END_RCPP
}
// move_red_cpp
NumericMatrix move_red_cpp(NumericMatrix m);
RcppExport SEXP _carsimr_geisler_move_red_cpp(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(move_red_cpp(m));
    return rcpp_result_gen;
END_RCPP
}
// move_blue_cpp
NumericMatrix move_blue_cpp(NumericMatrix m);
RcppExport SEXP _carsimr_geisler_move_blue_cpp(SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(move_blue_cpp(m));
    return rcpp_result_gen;
END_RCPP
}
// simulate_grid_cpp_c
List simulate_grid_cpp_c(NumericMatrix m, int trials);
RcppExport SEXP _carsimr_geisler_simulate_grid_cpp_c(SEXP mSEXP, SEXP trialsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    Rcpp::traits::input_parameter< int >::type trials(trialsSEXP);
    rcpp_result_gen = Rcpp::wrap(simulate_grid_cpp_c(m, trials));
    return rcpp_result_gen;
END_RCPP
}
// move_cars_cpp_c
List move_cars_cpp_c(double rho, int r, int c, double p, int trials);
RcppExport SEXP _carsimr_geisler_move_cars_cpp_c(SEXP rhoSEXP, SEXP rSEXP, SEXP cSEXP, SEXP pSEXP, SEXP trialsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type rho(rhoSEXP);
    Rcpp::traits::input_parameter< int >::type r(rSEXP);
    Rcpp::traits::input_parameter< int >::type c(cSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type trials(trialsSEXP);
    rcpp_result_gen = Rcpp::wrap(move_cars_cpp_c(rho, r, c, p, trials));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_carsimr_geisler_rho_value", (DL_FUNC) &_carsimr_geisler_rho_value, 3},
    {"_carsimr_geisler_randomShuffle", (DL_FUNC) &_carsimr_geisler_randomShuffle, 1},
    {"_carsimr_geisler_generate_and_shuffle", (DL_FUNC) &_carsimr_geisler_generate_and_shuffle, 3},
    {"_carsimr_geisler_initialize_grid_cpp", (DL_FUNC) &_carsimr_geisler_initialize_grid_cpp, 5},
    {"_carsimr_geisler_move_red_cpp", (DL_FUNC) &_carsimr_geisler_move_red_cpp, 1},
    {"_carsimr_geisler_move_blue_cpp", (DL_FUNC) &_carsimr_geisler_move_blue_cpp, 1},
    {"_carsimr_geisler_simulate_grid_cpp_c", (DL_FUNC) &_carsimr_geisler_simulate_grid_cpp_c, 2},
    {"_carsimr_geisler_move_cars_cpp_c", (DL_FUNC) &_carsimr_geisler_move_cars_cpp_c, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_carsimr_geisler(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
// #include <Rcpp.h>
// #include <random>
// #include <iostream>
// using namespace Rcpp;
//
// //' C++ version of initialize_grid.
// //'
// //' @import MASS
// //' @param rho The proportion of cars to fill the grid rounded to the nearest
// //' whole number
// //' @param r The number of rows in the grid
// //' @param c The number of columns in the grid
// //' @param p The proportion of blue and red cars as determined by the Bernoulli
// //' distribution
// //' @exportClass carsimr
// //' @export
// // [[Rcpp::export]]
// NumericMatrix initialize_grid(int rho, int r, int c, int p) {
//   if (rho >= 1) {
//     number_cars = int rho;
//   } else {
//     number_cars = int (rho * r * c);
//   }
//   cars = round(number_cars);
//   std::mt19937 gen;
//   std::bernoulli_distribution d(p);
//   gen.seed(777);
//   int car_allocation = d(gen);
//   car_allocation = car_allocation + 1;
//
// }
//
//
// // You can include R code blocks in C++ files processed with sourceCpp
// // (useful for testing and development). The R code will be automatically
// // run after the compilation.
// //
//
// /*** R
// timesTwo(42)
// */

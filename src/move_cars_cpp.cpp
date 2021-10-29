#include <Rcpp.h>
#include <random>
#include <iostream>
#include <algorithm>
using namespace Rcpp;

//' rho_value
//'
//' Returns rounded value of rho that can be passed to the number_cars variable.
//' @param x The proportion or number of cars to fill the matrix.
//' @param row The number of rows.
//' @param col The number of columns.
// [[Rcpp::export]]
int rho_value(double x, int row, int col) {
  if (x >= 1) return round(x) ;
  else return round(x * row * col) ;
}

//' "inline" allows us to make copies of this function where we need it. This
//' is useful because random_shuffle() needs a dynamic function, not a static
//' vector, in order create a random number stream based on one from R.

inline int randWrapper(const int n) { return floor(unif_rand() * n) ; }

//' randomShuffle
//'
//' Shuffles values of a vector randomly.
//' @param a A numeric vector.
// [[Rcpp::export]]
NumericVector randomShuffle(NumericVector a) {
  NumericVector b = clone(a) ;
  std::random_shuffle(b.begin(), b.end(), randWrapper) ;
  return b;
}

//' generate_and_shuffle
//'
//' Try to shuffle things
//' @param n an integer with the number of things we need to simulate
//' @param n2 an integer with the number of non-zero elements
//' @param p the probability of selecting a one (versus a two)
// [[Rcpp::export]]
NumericVector generate_and_shuffle(int n, int n2, double p) {
  NumericVector tvec(n) ;
  n2 = std::min(n, n2) ;
  int ones = R::rbinom(n2, p) ;
  for(int i = 0 ; i < ones ; i++) {tvec(i) = 2 ;}
  for(int i = ones ; i < n2 ; i++) {tvec(i) = 1 ;}
  return randomShuffle(tvec) ;
}

//' C++ version of initialize_grid.
//'
//' @import MASS
//' @param rho The proportion of cars to fill the grid rounded to the nearest
//' whole number
//' @param r The number of rows in the grid
//' @param c The number of columns in the grid
//' @param p The proportion of blue and red cars as determined by the Bernoulli
//' distribution
//' @exportClass carsimr
//' @export
// [[Rcpp::export]]
NumericMatrix initialize_grid_cpp(double rho, int r, int c, double p, int seed) {
  int number_cars = rho_value(rho, r, c) ;
  std::mt19937 gen ;
  gen.seed(seed) ;
  NumericVector vec = generate_and_shuffle(r * c, number_cars, p) ;
  vec.attr("dim") = Dimension(r, c) ;
  NumericMatrix grid2 = as<NumericMatrix>(vec) ;
  return grid2 ;
}

//' move_red_cpp
//'
//' C++ version of move_red_cars
//'
//' @param m A numeric matrix.
// [[Rcpp::export]]
NumericMatrix move_red_cpp(NumericMatrix m) {
  int row = m.rows() ;
  int col = m.cols() ;
  NumericMatrix m_new = Rcpp::clone(m) ;
  for (int i = 0 ; i < row ; i++) {
    for (int j = 0 ; j < col ; j++) {
      if (m(i, j) == 2) {
        if (j == col - 1) {
          if (m(i, 0) == 0) {
            m_new(i, j) = 0 ;
            m_new(i, 0) = 2 ;
          }
        } if (j < col - 1) {
          if (m(i, j + 1) == 0) {
            m_new(i, j) = 0 ;
            m_new(i, j + 1) = 2 ;
          }
        }
      }
    }
  }
  return m_new ;
}

//' move_blue_cpp
//' C++ version of move_blue_cars.
//'
//' @param m A matrix of blue and red cars.
// [[Rcpp::export]]
NumericMatrix move_blue_cpp(NumericMatrix m) {
  int row = m.rows() ;
  int col = m.cols() ;
  NumericMatrix m_new = Rcpp::clone(m) ;
  for (int i = 0 ; i < row ; i++) {
    for (int j = 0 ; j < col ; j++) {
      if (m(i, j) == 1) {

        if (i > 0) {
          if (m(i - 1, j) == 0) {
            m_new(i - 1, j) = 1 ;
            m_new(i, j) = 0 ;
          }
        } if (i == 0) {
          if (m(row - 1, j) == 0) {
            m_new(row - 1, j) = 1 ;
            m_new(i, j) = 0 ;
          }
        }
      }
    }
  }
  return m_new ;
}

//' simulate_grid_cpp
//'
//' C++ version of simulate_grid.
//'
//' @param move_red_cars Calls the 'move_red_cars' function notated above.
//' @param move_blue_cars Calls the 'move_blue_cars' function notated above.
//' @param trials Specifies the number of times the cars move on the grid.
//' @export
// [[Rcpp::export]]
List simulate_grid_cpp(NumericMatrix m, int trials) {
  List grid_states(trials + 1) ;
  grid_states(0) = m ;

  for (int i = 1 ; i < trials + 1 ; i++) {
    if((i - 1) % 2) {
      grid_states(i) = move_red_cpp(grid_states(i - 1)) ;
    } else {
      grid_states(i) = move_blue_cpp(grid_states(i - 1)) ;
      }
    }
  return grid_states ;
  }

//' move_cars_cpp
//'
//' C++ version of move_cars
//'
//' @param rho The proportion or number of cars in a grid.
//' @param r The number of rows in the grid.
//' @param c The number of columns in the grid.
//' @param p The proportion of red cars in the grid.
//' @param trials The number of grid movements to be simulated.
//' @export
// [[Rcpp::export]]
List move_cars_cpp(double rho, int r, int c, double p, int trials) {
  NumericMatrix mat = initialize_grid_cpp(rho, r, c, p, 777) ;
  List grids = simulate_grid_cpp(mat, trials) ;
  return grids ;
}

/*** R

*/

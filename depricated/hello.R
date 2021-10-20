#' initialize_grid
#'
#' Initializes the grid of red and blue cars.
#'
#' Using a specified proportion and grid size, we can randomly generate a matrix of
#' red and blue cars using the Bernoulli distribution. The seed has been set for
#' reproduceability.
#'
#' @param rho The proportion of cars to fill the grid rounded to the nearest whole
#' number
#' @param r The number of rows in the grid
#' @param c The number of columns in the grid
#' @param p The proportion of blue and red cars as determined by the Bernoulli
#' distribution
#' @exportClass carsimr
#' @export

initialize_grid <- function(rho, r, c, p) {
  set.seed(777)
  number.cars <- rho*r*c
  number.cars <- round(number.cars, digits = 0)
  car.allocation <- Rlab::rbern(number.cars, p)
  m <- matrix(NA, nrow = r, ncol = c)
  m[sample.int(length(m), length(car.allocation))] <- car.allocation
  class(m) <- "carsimr"
  m
}

#' plot.carsimr
#'
#' This takes the matrix generated of class carsimr and converts it to an image of
#' red and blue cars on the grid.
#'
#' @param x A matrix of class "carsimr"
#' @param y Optional if x is complete
#' @param col A vector to specify the colors of the cars
#' @param ... A symbol meaning that other arguments can be used.
#' @export

plot.carsimr <- function(x, y, col = c("red", "blue"), ...){
  class(x) <- "matrix"
  image(x, col = col)
}



#' initialize_grid
#'
#' Initializes the grid of red and blue cars.
#'
#' Using a specified proportion and grid size, we can randomly generate a matrix
#' of red and blue cars using the Bernoulli distribution. The seed has been
#' set for reproduceability.
#'
#' @import MASS
#' @param rho The proportion of cars to fill the grid rounded to the nearest
#' whole number
#' @param r The number of rows in the grid
#' @param c The number of columns in the grid
#' @param p The proportion of blue and red cars as determined by the Bernoulli
#' distribution
#' @examples
#' init_grid <- initialize_grid(rho = 0.3, r = 10, c = 10, p = 0.5)
#' @exportClass carsimr
#' @export

initialize_grid <- function(rho, r, c, p) {
  ifelse(rho >= 1, number_cars <- rho, number_cars <- rho * r * c)
  number_cars <- round(number_cars)
  set.seed(777)
  car_allocation <- Rlab::rbern(number_cars, p)
  mat <- matrix(3, r, c)
  mat[sample.int(length(mat), length(car_allocation))] <- car_allocation
  mat <- replace(mat, mat == 1, 2)
  mat <- replace(mat, mat == 0, 1)
  mat <- replace(mat, mat == 3, 0)
  class(mat) <- "carsimr"
  mat
}

#' plot.carsimr
#'
#' This takes the matrix generated of class carsimr and converts it to an
#' image of red and blue cars on the grid.
#'
#' @importFrom graphics image
#' @param x A matrix of class "carsimr"
#' @param y Optional if x is complete
#' @param ... A symbol meaning that other arguments can be used.
#' @export

plot.carsimr <- function(x, y, ...) {
  class(x) <- "matrix"
  image(t(x[rev(seq_len(nrow(x))), ]),
    col = c("white", "blue", "red"), xaxt = "n", yaxt = "n",
    breaks = c(-.5, .5, 1.5, 2.5)
  )
}

# intermediate functions

#------------------------------------------------------------------------------
# Return column number of an element
#------------------------------------------------------------------------------

#' col.number
#' @param x An element of a matrix.
#' @param y The matrix from which x is located.

col.number <- function(x, y) {
  return(ceiling(x / nrow(y)))
}

#------------------------------------------------------------------------------
# Return row number of an element
#------------------------------------------------------------------------------

#' row.number
#' @param x An element of a matrix.
#' @param y The matrix from which x is located.

row.number <- function(x, y) {
  row.index <- x %% nrow(y)
  ifelse(row.index == 0, row <- nrow(y), row <- row.index)
  row
}

#------------------------------------------------------------------------------
# move red cars
#------------------------------------------------------------------------------

#' move_red_cars
#' @param m A matrix to be passed onto the function. The end result will show
#' the movement of red cars (2's).

move_red_cars <- function(m) {
  m_new <- m
  red <- which(m == 2)
  for (i in seq_len(length(red))) {
    if (col.number(red[i], m) == ncol(m) & m[row.number(red[i], m), 1] == 0) {
      seqinr::swap(m_new[red[i]], m_new[row.number(red[i], m), 1])
    } else if (col.number(red[i], m) < ncol(m)) {
      if (m[row.number(red[i], m), col.number(red[i], m) + 1] == 0) {
        seqinr::swap(m_new[red[i]], m_new[
          row.number(red[i], m),
          col.number(red[i], m) + 1
        ])
      }
    }
  }
  return(m_new)
}

#------------------------------------------------------------------------------
# move blue cars
#------------------------------------------------------------------------------

#' move_blue_cars
#' @param m A matrix to be passed onto the function. The end result will show
#' the movement of blue cars (1's).
#'

move_blue_cars <- function(m) {
  m_new <- m
  blue <- which(m == 1)
  for (i in seq_len(length(blue))) {
    if (row.number(blue[i], m) == 1 & m[nrow(m), col.number(blue[i], m)] == 0) {
      seqinr::swap(m_new[blue[i]], m_new[nrow(m), col.number(blue[i], m)])
    } else if (row.number(blue[i], m) > 1) {
      if (m[blue[i] - 1] == 0) {
        seqinr::swap(m_new[blue[i] - 1], m_new[blue[i]])
      }
    }
  }
  return(m_new)
}

#------------------------------------------------------------------------------
# Simulating Car Movement
#------------------------------------------------------------------------------

#' simulate_grid
#' @param matrix A matrix to be passed onto the function that will demonstrate
#' car movement.
#' @param move_red_cars Calls the 'move_red_cars' function notated above.
#' @param move_blue_cars Calls the 'move_blue_cars' function notated above.
#' @param trials Specifies the number of times the cars move on the grid.

simulate_grid <- function(matrix, move_red_cars, move_blue_cars, trials) {
  grid_states <- vector("list", trials + 1)
  grid_states[[1]] <- matrix

  for (i in 2:(trials + 1)) {
    if ((i - 1) %% 2) {
      grid_states[[i]] <- move_blue_cars(grid_states[[i - 1]])
      class(grid_states[[i]]) <- "carsimr"
    } else {
      grid_states[[i]] <- move_red_cars(grid_states[[i - 1]])
      class(grid_states[[i]]) <- "carsimr"
    }
  }

  class(grid_states) <- "carsimrlist"
  grid_states
}

#' move_cars
#'
#' Provides a number of simulations of red & blue car movement on the grid.
#'
#' On odd iterations, blue cars will be moved vertically. On even iterations,
#' red cars will be moved horizontally. Each iteration will be plotted as a
#' new matrix so that the car movement can be visualized.
#'
#' @param rho the proportion of cars to fill the grid, rounded to the nearest
#' whole number.
#'
#' @param r the number of rows in the grid.
#' @param c the number of columns in the grid.
#' @param p the proportion of blue cars. 1-p represents the proportion of red
#' cars.
#' @param trials the number of simulations to be run on the grid.
#' @examples
#' car_simulation <- move_cars(rho = 0.4, r = 9, c = 9, p = 0.7, trials = 10)
#' @exportClass carsimrlist
#' @export

move_cars <- function(rho, r, c, p, trials) {
  mat <- initialize_grid(rho, r, c, p)
  simulate_grid(mat, move_red_cars, move_blue_cars, trials)
}

#' as.carsimr
#'
#' Converts matrices to class "carsimr"
#'
#' @param x A numeric matrix
#' @param ... Optional.
#'
as.carsimrlist <- function(x, ...) {
  structure(x, class = "carsimrlist")
}

#' simulate_grid_cpp
#'
#' A wrapper function that converts simulate_grid_cpp to class "carsimr"
#'
#' @param m a numeric matrix.
#' @param trials The number of car movement iterations.

simulate_grid_cpp <- function(m, trials) {
  car_sim <- simulate_grid_cpp_c(m, trials)
  for (i in seq_len(length(car_sim))) {
    class(car_sim[[i]]) <- "carsimr"
  }
  class(car_sim) <- "carsimrlist"
  car_sim
}

#' c++ functions return a list of matrices, we want to make a carsimr object
#' that looks like the one we made originally.
#' @param x a list of matrices.

carsimr_convert <- function(x) {
  dims <- dim(x[[1]])
  trials <- length(x)
  results <- matrix(0, prod(dims), trials)
  for (i in seq_len(trials)) {
    results[, i] <- as.vector(x[[i]])
  }
  as.carsimrlist(results, dims[1], dims[2])
}

#' Wrapper to c++ functions.
#'
#' @param rho A proportion between 0 and 1 of the number of grids that
#'   should be filled with cars. Alternatively, an integer value specifying
#'   the precise number of cars.
#' @param r The number of rows in the car grid.
#' @param c The number of columns in the car grid.
#' @param p The probability (between 0 and 1) of selecting a blue car.
#'   This means the probability of selecting a red car is 1-p.
#' @param trials The number of grid moves to store.
#'
#' @export

move_cars_cpp <- function(rho, r, c, p, trials) {
  car_moves <- move_cars_cpp_c(rho, r, c, p, trials)
  carsimr_convert(car_moves)
}

#' plot.carsimrlist
#'
#' Plots each iteration of car movement. In total, there will be 'trials+1'
#' matrices returned.
#'
#' @param x A list containing the matrices from each iteration.
#' @param y Ignored
#' @param ... Ignored
#' @export

plot.carsimrlist <- function(x, y, ...) {
  for (k in seq_len(length(x))) {
    plot(x[[k]])
    Sys.sleep(0.2)
  }
}

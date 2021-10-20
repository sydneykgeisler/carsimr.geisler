#' move_cars
#'
#' Provides a number of simulations of red & blue car movement on the grid.
#'
#' On odd iterations, blue cars will be moved vertically. On even iterations, red
#' cars will be moved horizontally. Each iteration will be plotted as a new matrix,
#' so that the car movement can be visualized.
#'
#' @param rho the proportion of cars to fill the grid, rounded to the nearest whole
#' number.
#' @param r the number of rows in the grid.
#' @param c the number of columns in the grid.
#' @param p the proportion of blue cars. 1-p represents the proportion of red cars.
#' @param trials the number of simulations to be run on the grid.
#' @exportClass carsimrlist
#' @export

move_cars <- function(rho, r, c, p, trials){
  number.cars <- rho*r*c
  number.cars <- round(number.cars)
  car.allocation <- Rlab::rbern(number.cars, p)
  mat <- matrix(2, r, c)
  mat[sample.int(length(mat), length(car.allocation))] <- car.allocation

  move_red_cars <- function(m){
    m_new = m
    for (i in 1:nrow(m)){
      for (j in 1:ncol(m)){
        if (m[i,j] == 1) next
        if (m[i,j] == 2) next
        else if (j < ncol(m)){
          if (m[i,j] == 0 & m[i,j+1] == 2) seqinr::swap(m_new[i,j], m_new[i,j+1])
        } else if (j == ncol(m)){
          if (m[i, ncol(m)] == 0 & m[i,1] == 2) seqinr::swap(m_new[i, ncol(m)], m_new[i,1])
        }
      }
    }
    return(m_new)
  }

  move_blue_cars <- function(m){
    m_new = m
    for (i in 1:nrow(m)){
      for (j in 1:ncol(m)){
        if (m[i,j] == 0) next
        if (m[i,j] == 2) next
        else if (i > 1){
          if (m[i,j] == 1 & m[i-1,j] == 2) seqinr::swap(m_new[i,j], m_new[i-1,j])
        } else if (i == 1){
          if (m[i,j] == 1 & m[nrow(m),j] == 2) seqinr::swap(m_new[i,j], m_new[nrow(m),j])
        }
      }
    }
    return(m_new)
  }

  grid_states <- vector("list", trials + 1)
  grid_states[[1]] <- mat

  for(i in 2:(trials+1)){
    if((i - 1) %% 2){
      grid_states[[i]] <- move_blue_cars(grid_states[[i - 1]])
    } else {
      grid_states[[i]] <- move_red_cars(grid_states[[i - 1]])
    }
  }

  class(mat) <- "carsimrlist"
  mat

}

#' plot.carsimrlist
#'
#' Plots each iteration of car movement. In total, there will be 'trials+1'
#' matrices returned.
#'
#' @param grid_states A list containing the matrices from each iteration.
#' @param col The vector of colors to be used in the matrix plots.
#' @param ... A miscellaneous variable to be specified.
#' @export

plot.carsimrlist <- function(grid_states, col = c("red", "blue", "white"), ...){
  for(i in seq_len(length(grid_states))){
    image(grid_states, col = col)
    Sys.sleep(0.2)
  }
}

move_red_cars2 <- function(m) {
  m_new <- m
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      if (m[i, j] == 0) {
        next
      } else if (m[i, j] == 1) {
        next
      } else if (j < ncol(m)) {
        if (m[i, j] == 2 & m[i, j + 1] == 0) seqinr::swap(m_new[i, j], m_new[i, j + 1])
      } else if (j == ncol(m)) {
        if (m[i, ncol(m)] == 2 & m[i, 1] == 0) seqinr::swap(m_new[i, ncol(m)], m_new[i, 1])
      }
    }
  }
  return(m_new)
}

move_blue_cars2 <- function(m) {
  m_new <- m
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      if (m[i, j] == 0) {
        next
      } else if (m[i, j] == 2) {
        next
      } else if (i > 1) {
        if (m[i, j] == 1 & m[i - 1, j] == 0) seqinr::swap(m_new[i, j], m_new[i - 1, j])
      } else if (i == 1) {
        if (m[i, j] == 1 & m[nrow(m), j] == 0) seqinr::swap(m_new[i, j], m_new[nrow(m), j])
      }
    }
  }
  return(m_new)
}

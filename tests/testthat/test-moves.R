test_that("simulate_grid works on test_1 matrices", {
  as_carsimr <- function(x) structure(x, class = "carsimr")
  expect_s3_class(as_carsimr(test_1), class = "carsimr")
  expect_equal(simulate_grid(
    test_1[[1]], move_red_cars,
    move_blue_cars, 1
  )[[2]], as_carsimr(test_1[[2]]))
  expect_equal(simulate_grid(
    test_1[[1]], move_red_cars,
    move_blue_cars, 2
  )[[3]], as_carsimr(test_1[[3]]))
})

test_that("simulate_grid works on test_2 matrices", {
  as_carsimr <- function(x) structure(x, class = "carsimr")
  expect_s3_class(as_carsimr(test_2), class = "carsimr")
  expect_equal(simulate_grid(
    test_2[[1]], move_red_cars,
    move_blue_cars, 1
  )[[2]], as_carsimr(test_2[[2]]))
  expect_equal(simulate_grid(
    test_2[[1]], move_red_cars,
    move_blue_cars, 2
  )[[3]], as_carsimr(test_2[[3]]))
})

test_that("simulate_grid works on test_3 matrices", {
  as_carsimr <- function(x) structure(x, class = "carsimr")
  expect_s3_class(as_carsimr(test_3), class = "carsimr")
  expect_equal(simulate_grid(
    test_3[[1]], move_red_cars,
    move_blue_cars, 1
  )[[2]], as_carsimr(test_3[[2]]))
  expect_equal(simulate_grid(
    test_3[[1]], move_red_cars,
    move_blue_cars, 2
  )[[3]], as_carsimr(test_3[[3]]))
})

test_that("simulate_grid works on test_4 matrices", {
  as_carsimr <- function(x) structure(x, class = "carsimr")
  expect_s3_class(as_carsimr(test_4), class = "carsimr")
  expect_equal(simulate_grid(
    test_4[[1]], move_red_cars,
    move_blue_cars, 1
  )[[2]], as_carsimr(test_4[[2]]))
  expect_equal(simulate_grid(
    test_4[[1]], move_red_cars,
    move_blue_cars, 2
  )[[3]], as_carsimr(test_4[[3]]))
})

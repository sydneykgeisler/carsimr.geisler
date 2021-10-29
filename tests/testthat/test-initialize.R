test_that("Red and blue are tabulating properly", {
  expect_false(2 %in% initialize_grid_cpp(rho = 0.3, r = 10, c = 15, p = 0))
  expect_false(1 %in% initialize_grid_cpp(rho = 0.3, r = 10, c = 15, p = 1))
})

test_that("Empty and Full Matrices", {
  expect_false((2 & 1) %in% initialize_grid_cpp(rho = 0, r = 10, c = 10, p = 0.5))
  expect_false(0 %in% initialize_grid_cpp(rho = 0.999, r = 10, c = 10, p = 0.5))
})

test_that("Rho tabulates properly", {
  set.seed(777)
  rho_temp <- runif(1, 2, 100)
  init_test <- initialize_grid_cpp(rho_temp, 10, 10, 0.5)
  expect_equal(sum(init_test != 0), round(rho_temp))
})

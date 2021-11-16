test_that("If rho < 0.5, do the number of empty cells surpass the number
          of cars?", {
  expect_gt(
    sum(initialize_grid_cpp(0.49, 10, 10, 0.5) == 0),
    sum(initialize_grid_cpp(0.49, 10, 10, 0.5) != 0)
  )
})

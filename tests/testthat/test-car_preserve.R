test_that("Numbers of each car stay constant", {
  expect_equal(
    sum(move_cars_cpp(0.5, 10, 15, 0.5, 11)[[1]] == 2),
    sum(move_cars_cpp(0.5, 10, 15, 0.5, 11)[[11]] == 2)
  )
  expect_equal(
    sum(move_cars_cpp(0.5, 10, 15, 0.5, 11)[[1]] == 1),
    sum(move_cars_cpp(0.5, 10, 15, 0.5, 11)[[11]] == 1)
  )
})

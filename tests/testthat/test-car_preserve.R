test_that("Numbers of each car stay constant", {
  sim <- move_cars_cpp_c(0.5, 10, 15, 0.5, 11)
  expect_equal(
    sum(sim[[1]] == 2),
    sum(sim[[11]] == 2)
  )
  expect_equal(
    sum(sim[[1]] == 1),
    sum(sim[[11]] == 1)
  )
})

test_that("get_all works", {
  fig <- Fig$new()
  fig$store_many(a = 1, b = 2, c = 3)
  expect_equal(fig$get_all(), list(a = 1, b = 2, c = 3))
})

test_that("fig_get_all works", {
  fig <- Fig$new()
  fig$store_many(a = 1, b = 2, c = 3)
  expect_equal(fig$get_all(), list(a = 1, b = 2, c = 3))
  fig_delete_all()
})

test_that("fig_get_all shares get_all arguments", {
  expect_equal(formalArgs(fig_get_all), formalArgs(Fig$new()$get_all))
})

test_that("load_config purges", {
  fig <- Fig$new()
  fig$store("a", 1)
  fig$load_config(list(b = 1), TRUE)
  expect_equal(fig$get("a"), NULL)
})

test_that("fig_load_config purges", {
  fig_store("a", 1)
  fig_load_config(list(b = 1), TRUE)
  expect_equal(fig_get("a"), NULL)
})

test_that("fig_load_config shares load_config arguments", {
  expect_equal(formalArgs(fig_load_config), formalArgs(Fig$new()$load_config))
})

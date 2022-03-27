test_that("load_config purges", {
  fig <- Fig$new()
  fig$set("a", 1)
  fig$load_config(list(b = 1), TRUE)
  expect_equal(fig$get("a"), NULL)
})

test_that("fig_load_config purges", {
  fig_set("a", 1)
  fig_load_config(list(b = 1), TRUE)
  expect_equal(fig_get("a"), NULL)
})

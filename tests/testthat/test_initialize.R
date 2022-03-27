test_that("initialize works", {
  expect_silent(Fig$new())
  expect_silent(Fig$new("foo"))
})

test_that("env_prefix argument works", {
  fig <- Fig$new("foo_")
  with_envvar(list(foo_bar = "a"), expect_equal(fig$get("bar"), "a"))
  expect_equal(fig$get("bar"), NULL)
})

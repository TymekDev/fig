test_that("initialize works", {
  expect_silent(Fig$new())
  expect_silent(Fig$new(env_prefix = "foo"))
  expect_silent(Fig$new(split_on = "_"))
})

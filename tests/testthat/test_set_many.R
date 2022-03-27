test_that("set_many validates ...", {
  fig <- Fig$new()
  expect_error(fig$set_many(1))
  expect_error(fig$set_many(a = 1, a = 2))
  expect_silent(fig$set_many(a = 1, b = 2))
})

test_that("set_many returns reference", {
  fig <- Fig$new()
  expect_identical(fig$set_many(foo = 1), fig)
})

test_that("set_many works", {
  fig <- Fig$new()
  env <- new.env()
  l <- list()
  fig$set_many(foo = 1, bar = env, baz = l)
  expect_equal(fig$get("foo"), 1)
  expect_identical(fig$get("bar"), env)
  expect_identical(fig$get("baz"), l)
})

test_that("set_many works with YAML key notation", {
  fig <- Fig$new()
  fig$set_many("foo.bar" = 1, "foo.bar.baz" = 2)
  expect_equal(fig$get("foo.bar"), list(1, baz = 2))
})

test_that("fig_set_many works", {
  env <- new.env()
  l <- list()
  fig_set_many(foo = 1, bar = env, baz = l)
  expect_equal(fig_get("foo"), 1)
  expect_identical(fig_get("bar"), env)
  expect_identical(fig_get("baz"), l)
  fig_purge()
})

test_that("fig_set_many shares set_many arguments", {
  expect_equal(formalArgs(fig_set_many), formalArgs(Fig$new()$set_many))
})

test_that("store_many validates ...", {
  fig <- Fig$new()
  expect_error(fig$store_many(1))
  expect_error(fig$store_many(a = 1, a = 2))
  expect_silent(fig$store_many(a = 1, b = 2))
})

test_that("store_many returns reference", {
  fig <- Fig$new()
  expect_identical(fig$store_many(foo = 1), fig)
})

test_that("store_many works", {
  fig <- Fig$new()
  env <- new.env()
  l <- list()
  fig$store_many(foo = 1, bar = env, baz = l)
  expect_equal(fig$get("foo"), 1)
  expect_identical(fig$get("bar"), env)
  expect_identical(fig$get("baz"), l)
})

test_that("store_many works with YAML key notation", {
  fig <- Fig$new()
  fig$store_many("foo.bar" = 1, "foo.bar.baz" = 2)
  expect_equal(fig$get("foo.bar"), list(1, baz = 2))
})

test_that("fig_store_many works", {
  env <- new.env()
  l <- list()
  fig_store_many(foo = 1, bar = env, baz = l)
  expect_equal(fig_get("foo"), 1)
  expect_identical(fig_get("bar"), env)
  expect_identical(fig_get("baz"), l)
  fig_purge()
})

test_that("fig_store_many shares store_many arguments", {
  expect_equal(formalArgs(fig_store_many), formalArgs(Fig$new()$store_many))
})

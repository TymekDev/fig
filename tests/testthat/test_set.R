test_that("set returns reference", {
  fig <- Fig$new()
  expect_identical(fig$set("foo", 1), fig)
})

test_that("set works", {
  fig <- Fig$new()

  fig$set("foo", 1)
  expect_equal(fig$get("foo"), 1)

  env <- new.env()
  fig$set("foo", env)
  expect_identical(fig$get("foo"), env)

  l <- list()
  fig$set("foo", l)
  expect_identical(fig$get("foo"), l)
})

test_that("set works with YAML key notation", {
  fig <- Fig$new()

  fig$set("foo.bar", 1)
  expect_equal(fig$get("foo"), list(bar = 1))
  expect_equal(fig$get("foo.bar"), 1)

  fig$set("foo.bar.baz", 2)
  expect_equal(fig$get("foo.bar"), list(1, baz = 2))

  fig$set("foo.bar.xyz", 3)
  expect_equal(fig$get("foo.bar"), list(1, baz = 2, xyz = 3))

  fig$set("foo.bar", 4)
  expect_equal(fig$get("foo"), list(bar = 4))
  expect_equal(fig$get("foo.bar"), 4)
})

test_that("fig_set works", {
  fig_set("foo", 1)
  expect_equal(fig_get("foo"), 1)

  env <- new.env()
  fig_set("foo", env)
  expect_identical(fig_get("foo"), env)

  l <- list()
  fig_set("foo", l)
  expect_identical(fig_get("foo"), l)

  fig_purge()
})

test_that("fig_set shares set arguments", {
  expect_equal(formalArgs(fig_set), formalArgs(Fig$new()$set))
})

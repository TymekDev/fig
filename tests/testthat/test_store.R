test_that("store returns reference", {
  fig <- Fig$new()
  expect_identical(fig$store("foo", 1), fig)
})

test_that("store works", {
  fig <- Fig$new()

  fig$store("foo", 1)
  expect_equal(fig$get("foo"), 1)

  env <- new.env()
  fig$store("foo", env)
  expect_identical(fig$get("foo"), env)

  l <- list()
  fig$store("foo", l)
  expect_identical(fig$get("foo"), l)
})

test_that("store works with YAML key notation", {
  fig <- Fig$new()

  fig$store("foo.bar", 1)
  expect_equal(fig$get("foo"), list(bar = 1))
  expect_equal(fig$get("foo.bar"), 1)

  fig$store("foo.bar.baz", 2)
  expect_equal(fig$get("foo.bar"), list(1, baz = 2))

  fig$store("foo.bar.xyz", 3)
  expect_equal(fig$get("foo.bar"), list(1, baz = 2, xyz = 3))

  fig$store("foo.bar", 4)
  expect_equal(fig$get("foo"), list(bar = 4))
  expect_equal(fig$get("foo.bar"), 4)

  fig <- Fig$new()
  fig$store_many("foo" = 1, "bar" = 2)
  fig$store_many("foo.bar.baz" = 1)
  expect_equal(fig$get("foo.bar"), list(baz = 1))
})

test_that("fig_store works", {
  fig_store("foo", 1)
  expect_equal(fig_get("foo"), 1)

  env <- new.env()
  fig_store("foo", env)
  expect_identical(fig_get("foo"), env)

  l <- list()
  fig_store("foo", l)
  expect_identical(fig_get("foo"), l)

  fig_delete_all()
})

test_that("fig_store shares store arguments", {
  expect_equal(formalArgs(fig_store), formalArgs(Fig$new()$store))
})

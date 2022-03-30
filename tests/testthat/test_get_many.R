test_that("get_many works", {
  fig <- Fig$new()
  env <- new.env()
  l <- list()
  fig$store_many(foo = 1, bar = env, baz = l)
  expect_equal(fig$get_many("foo", "bar", "baz"), list(1, env, l))
})

test_that("get_many works with YAML key notation", {
  fig <- Fig$new()
  fig$store_many("foo.bar" = 1, "foo.bar.baz" = 2)
  expect_equal(
    fig$get_many("foo.bar", "foo.bar.baz"),
    list(list(1, baz = 2), 2)
  )
})

test_that("fig_get_many works", {
  env <- new.env()
  l <- list()
  fig_store_many(foo = 1, bar = env, baz = l)
  expect_equal(fig_get_many("foo", "bar", "baz"), list(1, env, l))
  fig_delete_all()
})

test_that("fig_get_many shares get_many arguments", {
  expect_equal(formalArgs(fig_get_many), formalArgs(Fig$new()$get_many))
})

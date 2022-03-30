test_that("configure returns reference", {
  fig <- Fig$new()
  expect_identical(fig$configure("foo"), fig)
})

test_that("configure validates env_prefix", {
  fig <- Fig$new()
  expect_error(fig$configure(env_prefix = c("a", "b")))
  expect_error(fig$configure(env_prefix = NA_character_))
  expect_error(fig$configure(env_prefix = 1))
  expect_error(fig$configure(env_prefix = list()))
  expect_silent(fig$configure(env_prefix = "foo"))
  expect_silent(fig$configure(env_prefix = ""))
})

test_that("configure works for env_prefix", {
  fig <- Fig$new()$configure(env_prefix = "foo_")
  with_envvar(list(foo_bar = "a"), expect_equal(fig$get("bar"), "a"))
  expect_equal(fig$get("bar"), NULL)
})

test_that("fig_configure works for env_prefix", {
  fig_configure(env_prefix = "foo_")
  with_envvar(list(foo_bar = "a"), expect_equal(fig_get("bar"), "a"))
  expect_equal(fig_get("bar"), NULL)
  fig_delete_all()
})

test_that("configure validates split_on", {
  fig <- Fig$new()
  expect_error(fig$configure(split_on = c("a", "b")))
  expect_error(fig$configure(split_on = NA_character_))
  expect_error(fig$configure(split_on = 1))
  expect_error(fig$configure(split_on = list()))
  expect_silent(fig$configure(split_on = "foo"))
  expect_silent(fig$configure(split_on = ""))
})

test_that("configure works for split_on", {
  fig <- Fig$new()$configure(split_on = "_")
  fig$store("foo_bar", 1)
  expect_equal(fig$get("foo_bar"), 1)

  fig$configure(split_on = "")
  fig$store("foo_bar", 2)
  expect_equal(fig$get("foo"), list(bar = 1))
  expect_equal(fig$get("foo_bar"), 2)
})

test_that("fig_configure works for split_on", {
  fig_configure(split_on = "_")
  fig_store("foo_bar", 1)
  expect_equal(fig_get("foo"), list(bar = 1))
  fig_delete_all()
})

test_that("fig_configure shares configure arguments", {
  expect_equal(
    formalArgs(fig_configure),
    formalArgs(Fig$new()$configure)
  )
})

test_that("update_env_prefix validates env_prefix", {
  fig <- Fig$new()
  expect_error(fig$update_env_prefix(c("a", "b")))
  expect_error(fig$update_env_prefix(NA_character_))
  expect_error(fig$update_env_prefix(1))
  expect_error(fig$update_env_prefix(list()))
  expect_silent(fig$update_env_prefix("foo"))
})

test_that("update_env_prefix returns reference", {
  fig <- Fig$new()
  expect_identical(fig$update_env_prefix("foo"), fig)
})

test_that("update_env_prefix works", {
  fig <- Fig$new()$update_env_prefix("foo_")
  with_envvar(list(foo_bar = "a"), expect_equal(fig$get("bar"), "a"))
  expect_equal(fig$get("bar"), NULL)
})

test_that("fig_update_env_prefix works", {
  fig_update_env_prefix("foo_")
  with_envvar(list(foo_bar = "a"), expect_equal(fig_get("bar"), "a"))
  expect_equal(fig_get("bar"), NULL)
  fig_purge()
})

test_that("fig_update_env_prefix shares update_env_prefix arguments", {
  expect_equal(
    formalArgs(fig_update_env_prefix),
    formalArgs(Fig$new()$update_env_prefix)
  )
})

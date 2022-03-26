# initialize works
expect_silent(Fig$new("a"))
expect_error(Fig$new(c("a", "b")))

fig <- Fig$new()

# set returns reference to original object
expect_identical(fig$set("a", 1), fig)
expect_identical(fig$set("b", 1:3), fig)
expect_identical(fig$set("c", list("x", 1)), fig)

# get retrieves correct value
expect_equal(fig$get("a"), 1)
expect_equal(fig$get("b"), 1:3)
expect_equal(fig$get("c"), list("x", 1))

# delete works
expect_identical(fig$delete("b"), fig)
expect_equal(fig$get("b"), NULL)

# get precedence works (environment > stored values)
if (requireNamespace("withr", quietly = TRUE)) {
  withr::with_envvar(list(a = 2), expect_equal(fig$get("a"), "2"))
  withr::with_envvar(list(asdf = 2), expect_equal(fig$get("a"), 1))
}

# update_env_prefix and env_prefix work
expect_error(fig$update_env_prefix(c("a", "b")))
expect_error(fig$update_env_prefix(NA_character_))
expect_error(fig$update_env_prefix(1))
expect_error(fig$update_env_prefix(list()))
expect_identical(fig$update_env_prefix("foo_"), fig)
if (requireNamespace("withr", quietly = TRUE)) {
  expect_equal(fig$get("a"), 1)
  withr::with_envvar(list(foo_a = 2), expect_equal(fig$get("a"), "2"))
  fig$update_env_prefix("bar_")
  withr::with_envvar(list(bar_a = 2), expect_equal(fig$get("a"), "2"))
}

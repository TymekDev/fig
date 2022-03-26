fig <- Fig$new()

# set returns reference to original object
expect_identical(fig$set("a", 1), fig)
expect_identical(fig$set("b", 1:3), fig)
expect_identical(fig$set("c", list("x", 1)), fig)

# get retrieves correct value
expect_equal(fig$get("a"), 1)
expect_equal(fig$get("b"), 1:3)
expect_equal(fig$get("c"), list("x", 1))

# get precedence works (environment > stored values)
if (requireNamespace("withr", quietly = TRUE)) {
  withr::with_envvar(list(a = 2), expect_equal(fig$get("a"), "2"))
  withr::with_envvar(list(asdf = 2), expect_equal(fig$get("a"), 1))
}

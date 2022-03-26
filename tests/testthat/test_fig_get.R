test_that("get precedence works", {
  fig <- Fig$new("prefix_")

  # System environment > manually set value
  fig$set("foo", 1)
  with_envvar(list(prefix_foo = "a"), expect_equal(fig$get("foo"), "a"))
})

test_that("get validates key", {
  fig <- Fig$new()
  expect_error(fig$get(c("a", "b")))
  expect_error(fig$get(list(1, 2)))
  expect_error(fig$get(""))
  expect_silent(fig$get("foo"))
})

test_that("get precedence works", {
  fig <- Fig$new("prefix_")

  # System environment > manually set value
  fig$set("foo", 1)
  with_envvar(list(prefix_foo = "a"), expect_equal(fig$get("foo"), "a"))
})

test_that("get works with YAML key notation", {
  fig <- Fig$new()

  fig$set("foo", list(bar = 1))
  expect_equal(fig$get("foo.bar"), 1)

  fig$set("foo", list(bar = list(baz = 2)))
  expect_equal(fig$get("foo"), list(bar = list(baz = 2)))
  expect_equal(fig$get("foo.bar"), list(baz = 2))
  expect_equal(fig$get("foo.bar.baz"), 2)
})

test_that("get environment lookup works with YAML key notation", {
  fig <- Fig$new("prefix_")
  fig$set("foo", list(bar = 1))
  with_envvar(list(prefix_foo_bar = "a"), expect_equal(fig$get("foo.bar"), "a"))
  expect_equal(fig$get("foo.bar"), 1)
})

test_that("get split argument works", {
  fig <- Fig$new()
  fig$set("foo", list(bar = 1))
  fig$set("foo.bar", 2, FALSE)
  expect_equal(fig$get("foo.bar"), 1)
  expect_equal(fig$get("foo.bar", FALSE), 2)
})

test_that("fig.split option works", {
  fig <- Fig$new()
  fig$set("foo", list(bar = 1))
  fig$set("foo.bar", 2, FALSE)
  expect_equal(fig$get("foo.bar"), 1)
  with_options(list(fig.split = FALSE), expect_equal(fig$get("foo.bar"), 2))
})

test_that("fig_get shares get arguments", {
  expect_equal(formalArgs(fig_get), formalArgs(Fig$new()$get))
})

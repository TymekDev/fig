test_that("delete returns reference", {
  fig <- Fig$new()
  expect_identical(fig$set("foo", 1)$delete("foo"), fig)
})

test_that("delete works", {
  fig <- Fig$new()
  expect_equal(fig$set("foo", 1)$delete("foo")$get("foo"), NULL)
  fig$set_many(foo = 1, bar = 2)$delete("foo", "bar")
  expect_equal(fig$get("foo"), NULL)
  expect_equal(fig$get("bar"), NULL)
})

test_that("fig_delete works", {
  fig_set("foo", 1)
  fig_delete("foo")
  expect_equal(fig_get("foo"), NULL)
  fig_purge()
})

test_that("fig_delete shares delete arguments", {
  expect_equal(formalArgs(fig_delete), formalArgs(Fig$new()$delete))
})

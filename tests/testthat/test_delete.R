test_that("delete returns reference", {
  fig <- Fig$new()
  expect_identical(fig$store("foo", 1)$delete("foo"), fig)
})

test_that("delete works", {
  fig <- Fig$new()
  expect_equal(fig$store("foo", 1)$delete("foo")$get("foo"), NULL)
  fig$store_many(foo = 1, bar = 2)$delete("foo", "bar")
  expect_equal(fig$get("foo"), NULL)
  expect_equal(fig$get("bar"), NULL)
})

test_that("fig_delete works", {
  fig_store("foo", 1)
  fig_delete("foo")
  expect_equal(fig_get("foo"), NULL)
  fig_delete_all()
})

test_that("fig_delete shares delete arguments", {
  expect_equal(formalArgs(fig_delete), formalArgs(Fig$new()$delete))
})

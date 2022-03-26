test_that("delete returns reference", {
  fig <- Fig$new()
  expect_identical(fig$set("foo", 1)$delete("foo"), fig)
})

test_that("delete works", {
  fig <- Fig$new()
  expect_equal(fig$set("foo", 1)$delete("foo")$get("foo"), NULL)
})

test_that("fig_delete works", {
  fig_set("foo", 1)
  fig_delete("foo")
  expect_equal(fig_get("foo"), NULL)
})

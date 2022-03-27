test_that("delete_all works", {
  fig <- Fig$new()
  old_items <- fig$.__enclos_env__$private$items
  fig$delete_all()
  expect_false(identical(fig$.__enclos_env__$private, old_items))
})

test_that("fig_delete_all shares delete_all arguments", {
  expect_equal(formalArgs(fig_delete_all), formalArgs(Fig$new()$delete_all))
})

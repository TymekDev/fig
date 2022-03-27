test_that("purge works", {
  fig <- Fig$new()
  old_items <- fig$.__enclos_env__$private$items
  fig$purge()
  expect_false(identical(fig$.__enclos_env__$private, old_items))
})

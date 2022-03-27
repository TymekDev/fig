test_that("store_list delete_alls", {
  fig <- Fig$new()
  fig$store("a", 1)
  fig$store_list(list(b = 1), TRUE)
  expect_equal(fig$get("a"), NULL)
})

test_that("fig_store_list delete_alls", {
  fig_store("a", 1)
  fig_store_list(list(b = 1), TRUE)
  expect_equal(fig_get("a"), NULL)
})

test_that("fig_store_list shares store_list arguments", {
  expect_equal(formalArgs(fig_store_list), formalArgs(Fig$new()$store_list))
})

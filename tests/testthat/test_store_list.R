# NOTE: store_list is tested via store_many (that came before it)
test_that("fig_store_list shares store_list arguments", {
  expect_equal(formalArgs(fig_store_list), formalArgs(Fig$new()$store_list))
})

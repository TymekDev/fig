test_that("fig_store_list works", {
  env <- new.env()
  l <- list()
  fig_store_list(list(foo = 1, bar = env, baz = l))
  expect_equal(fig_get("foo"), 1)
  expect_identical(fig_get("bar"), env)
  expect_identical(fig_get("baz"), l)
  fig_delete_all()
})

# NOTE: store_list is tested via store_many (that came before it)
test_that("fig_store_list shares store_list arguments", {
  expect_equal(formalArgs(fig_store_list), formalArgs(Fig$new()$store_list))
})

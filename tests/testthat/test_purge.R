test_that("purge works", {
  old_fig <- fig:::global_fig()
  purge()
  expect_false(identical(fig:::global_fig(), old_fig))
})

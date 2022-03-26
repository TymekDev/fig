# fig_set returns reference to internal fig object
expect_identical(fig_set("a", 1), fig:::global_fig())

# fig_get retrieves correct value
expect_equal(fig_get("a"), 1)

# purge creates a new Fig instance
old_fig <- fig:::global_fig()
purge()
expect_false(identical(fig:::global_fig(), old_fig))

# fig_delete works
fig_set("a", 1)
expect_identical(fig_delete("a"), fig:::global_fig())
expect_equal(fig_get("a"), NULL)

# fig_update_env_prefix works
if (requireNamespace("withr", quietly = TRUE)) {
  expect_silent(fig_update_env_prefix("xyz_"))
  withr::with_envvar(list(xyz_a = 2), expect_equal(fig_get("a"), "2"))
  fig_set("a", 1)
  withr::with_envvar(list(xyz_a = 2), expect_equal(fig_get("a"), "2"))
}

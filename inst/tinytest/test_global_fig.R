# fig_set returns reference to internal fig object
expect_identical(fig_set("a", 1), fig:::global_fig())

# fig_get retrieves correct value
expect_equal(fig_get("a"), 1)

# purge creates a new Fig instance
old_fig <- fig:::global_fig()
purge()
expect_false(identical(fig:::global_fig(), old_fig))

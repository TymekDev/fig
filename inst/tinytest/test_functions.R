# fig_set returns reference to internal fig object
expect_identical(fig_set("a", 1), fig:::fig)

# fig_get retrieves correct value
expect_equal(fig_get("a"), 1)

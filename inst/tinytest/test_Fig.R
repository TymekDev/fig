fig <- Fig$new()

# set returns reference to original object
expect_identical(fig$set("a", 1), fig)

# get retrieves correct value
expect_equal(fig$get("a"), 1)

fig <- Fig$new()

#' @title Configure the Global Fig Instance
#' @description This function allows modifying configuration of the global fig.
#' Refer to argument descriptions for available options.
#' @details Unset arguments do not change configuration.
#' @param env_prefix (character) A prefix to be prepended to a key before
#' system environment lookup. Pass an empty string to reset.
#' @param split_on (character) A value to split keys on. See Details
#' section in [`Fig`]. Providing an empty string disables this behavior.
#' @return Reference to the global fig instance. Other methods can be chained
#' after this one.
#' @examples
#' fig_configure(env_prefix = "foo_")
#' fig_configure(split_on = "")
#' fig_configure() # has no effect
#' @export
fig_configure <- function(env_prefix, split_on) {
  fig$configure(env_prefix, split_on)
}

#' @title Delete Stored Values
#' @description These functions allow deleting values stored in the global fig
#' instance.
#' @param ... Keys to be deleted.
#' @return Reference to the global fig instance. Other methods can be chained
#' after this one.
#' @examples
#' fig_store_many("foo" = 1, "bar" = 2, "baz" = 3)
#' fig_delete("foo")
#' fig_delete("bar", "baz")
#' fig_get_many("foo", "bar", "baz") # == list(NULL, NULL, NULL)
#' @export
fig_delete <- function(...) {
  fig$delete(...)
}

#' @examples
#' fig_store_many("foo" = 1, "bar" = 2, "baz" = 3)
#' fig_delete_all()
#' fig_get_many("foo", "bar", "baz") # == list(NULL, NULL, NULL)
#' @rdname fig_delete
#' @export
fig_delete_all <- function() {
  fig$delete_all()
}

#' @title Retrieve Stored Values
#' @description These functions allow retrieving values stored in the global fig
#' instance.
#' @details These functions return values based on a following priority
#' (highest to lowest). If value is not found, then it looks up next level
#' in the precedence.
#' 1. System environment variable (case sensitive)
#' 1. Value manually set
#'
#' For system environment lookup dots are replaced by underscores, e.g.
#' `fig_get("foo.bar")` will look up __foo_bar__.
#' @param key A key to retrieve a value for.
#' @return A value associated with provided `key`.
#' @examples
#' fig_store("foo", 1)
#' fig_get("foo")
#'
#' fig_store("bar", list(baz = 2))
#' fig_get("bar.baz")
#'
#' fig_configure(split_on = "")
#' fig_get("bar.baz") # == NULL
#' @export
fig_get <- function(key) {
  fig$get(key)
}

#' @param ... Keys to retrieve values for.
#' @return An unnamed list of values associated with keys provided in `...`.
#' @examples
#' fig_store_many(foo = 1, bar = 2, baz = 3)
#' fig_get_many("foo", "bar")
#' @rdname fig_get
#' @export
fig_get_many <- function(...) {
  fig$get_many(...)
}

#' @return An unnamed list of all stored values.
#' @examples
#' fig_store_many(foo = 1, bar = 2, baz = 3)
#' fig_get_all()
#' @rdname fig_get
#' @export
fig_get_all <- function() {
  fig$get_all()
}

#' @title Store Values
#' @description These functions allow storing values in the global fig instance.
#' @param key A key to store a value for.
#' @param value A value to be stored.
#' @return Reference to self. Other methods can be chained after this one.
#' @examples
#' fig_store("foo", 1)
#' fig_store("bar", 123)$store("baz", list(1, 2, 3))
#' fig_store("x.y", "a")
#' @export
fig_store <- function(key, value) {
  fig$store(key, value)
}

#' @param l (named list) Names are used as keys for storing their values.
#' @examples
#' fig_store_list(list(foo = 123, bar = "abc"))
#' @rdname fig_store
#' @export
fig_store_list <- function(l) {
  fig$store_list(l)
}

#' @param ... Named arguments. Names are used as keys for storing argument
#' values.
#' @examples
#' fig_store_many("foo" = 1, "bar" = 2)
#' fig_store_many("foo.bar.baz" = 1)
#' fig_store_many("foo" = "a", "baz" = 123)
#' @rdname fig_store
#' @export
fig_store_many <- function(...) {
  fig$store_many(...)
}

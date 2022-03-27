#' @title Create a Fig to Store Config
#'
#' @description `Fig` class is a main driver of this package. For usage details
#' refer to `Fig` methods documentation.
#'
#' Fig provides a set of exported functions. This makes `Fig` class instance
#' creation optional, and makes the package mimic being a class instance. Those
#' functions are simple wrappers on an internal `Fig` object and are prefixed to
#' avoid masking.
#'
#' @export
Fig <- R6::R6Class( # nolint
  classname = "Fig",
  public = list(
    #' @description Create a new Fig instance
    #' @param env_prefix (character) A prefix to be prepended to a key before
    #' system environment lookup.
    #' @examples
    #' fig <- Fig$new()
    #' fig <- Fig$new("RCONNECT_")
    initialize = function(env_prefix = "") {
      self$update_env_prefix(env_prefix)
      private$items <- new.env()
    },

    #' @description Delete a stored value
    #' @param key A key to delete its corresponding value.
    #' @examples
    #' fig <- Fig$new()
    #' fig$set("foo", 1)
    #' fig$get("foo") # == 1
    #' fig$delete("foo")
    #' fig$get("foo") # == NULL
    delete = function(key) {
      rm(list = key, envir = private$items)
      invisible(self)
    },

    #' @description Get a value
    #' @details This function returns values based on a following priority
    #' (highest to lowest). If value is not found, then it looks up next level
    #' in the precedence.
    #' 1. System environment variable (case sensitive)
    #' 1. Value manually set
    #'
    #' For system environment lookup dots are replaced by underscores, e.g.
    #' `fig$get("foo.bar")` will look up __foo_bar__.
    #'
    #' Additionally, Fig treats dots in `key` as nest level delimiters.
    #' Therefore, `fig$get("foo.bar")` is equivalent to `fig$get("foo")$bar`.
    #' This behavior can be disabled either by setting `options(fig.split =
    #' FALSE)` or by providing `split = FALSE` argument.
    #' @param key A key to retrieve its corresponding value.
    #' @param split A logical determining whether dots in `key` are treated
    #' specially or as is. See Details section.
    #' @examples
    #' fig <- Fig$new()
    #' fig$set("foo", 1)
    #' fig$get("foo")
    #'
    #' fig$set("bar", list(baz = 2))
    #' fig$get("bar.baz")
    #'
    #' fig$set("bar.baz", 3)
    #' fig$get("bar.baz") # == 2
    #' fig$get("bar.baz", split = FALSE) # == 3
    get = function(key, split = getOption("fig.split", TRUE)) {
      stopifnot(length(key) == 1, nchar(key) > 0)
      env_key <- sub(".", "_", key, fixed = TRUE)
      value <- Sys.getenv(private$add_env_prefix(env_key), NA)
      if (!is.na(value)) {
        return(value)
      }
      if (isTRUE(split)) private$traverse_items(key) else private$items[[key]]
    },

    #' @description Store a value
    #' @param key A key to store a value for.
    #' @param value A value to be stored.
    #' @examples
    #' fig <- Fig$new()
    #' fig$set("foo", 1)
    #' fig$set("bar", 123)$set("baz", list(1, 2, 3))
    set = function(key, value) {
      private$items[[key]] <- value
      invisible(self)
    },

    #' @description Set any number of values at once
    #' @param ... Named values Names are used as keys for provided values.
    #' @examples
    #' fig <- Fig$new()
    #' fig$set_many("foo" = 1, "bar" = 2)
    set_many = function(...) {
      args <- list(...)
      keys <- names(args)
      stopifnot(
        !is.null(keys),
        all(keys != ""),
        length(unique(keys)) == length(keys)
      )
      for (key in keys) {
        self$set(key, args[[key]])
      }
      invisible(self)
    },

    #' @description Update prefix for system environment variables
    #' @param env_prefix (character) A prefix to be prepended to a key before
    #' system environment lookup.
    #' @examples
    #' fig <- Fig$new()
    #' fig$update_env_prefix("RCONNECT_")
    update_env_prefix = function(env_prefix) {
      stopifnot(
        is.character(env_prefix),
        !is.na(env_prefix),
        length(env_prefix) == 1
      )
      private$add_env_prefix <- if (nchar(env_prefix) > 0) {
        function(key) paste0(env_prefix, key)
      } else {
        identity
      }
      invisible(self)
    }
  ),
  private = list(
    add_env_prefix = NULL,
    items = NULL,
    traverse_items = function(key) {
      value <- private$items
      for (key_part in strsplit(key, ".", TRUE)[[1]]) {
        value <- value[[key_part]]
      }
      value
    }
  )
)

#' @param key A key to delete its corresponding value.
#' @rdname Fig
#' @export
fig_delete <- function(key) {
  global_fig()$delete(key)
}

#' @param key A key to retrieve its corresponding value.
#' @param split A logical determining whether dots in `key` are treated
#' specially or as is. See Details section.
#' @rdname Fig
#' @export
fig_get <- function(key, split = getOption("fig.split", TRUE)) {
  global_fig()$get(key)
}

#' @param key A key to store a value for.
#' @param value A value to be stored.
#' @rdname Fig
#' @export
fig_set <- function(key, value) {
  global_fig()$set(key, value)
}

#' @description Set any number of values at once
#' @param ... Named values Names are used as keys for provided values.
#' @rdname Fig
#' @export
fig_set_many <- function(...) {
  global_fig()$set_many(...)
}

#' @param env_prefix (character) A prefix to be prepended to a key before system
#' environment lookup.
#' @rdname Fig
#' @export
fig_update_env_prefix <- function(env_prefix) {
  global_fig()$update_env_prefix(env_prefix)
}

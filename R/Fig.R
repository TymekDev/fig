#' @title Fig for Your Config
#'
#' @description `Fig` class is a main driver of this package. For usage details
#' refer to `Fig` class methods documentation.
#'
#' Fig provides a set of exported functions. This makes `Fig` class instance
#' creation optional, and makes the package itself mimic being a class instance.
#' Those functions are wrappers on an internal `Fig` object.
#'
#' @export
Fig <- R6::R6Class( # nolint
  classname = "Fig",
  public = list(
    #' @description Create a New Fig Instance
    #' @param env_prefix (character) A prefix to be prepended to a key before
    #' system environment lookup.
    #' @return New instance of `Fig`.
    #' @examples
    #' fig <- Fig$new()
    #' fig <- Fig$new(env_prefix = "RCONNECT_")
    initialize = function(env_prefix = "") {
      self$configure(env_prefix = env_prefix)
    },

    #' @description Configure a Fig Instance
    #' @details Unset arguments do not change configuration.
    #' @param env_prefix (character) A prefix to be prepended to a key before
    #' system environment lookup. Pass an empty string to reset.
    #' @return Reference to self. Other methods can be chained after this one.
    #' @examples
    #' fig <- Fig$new(env_prefix = "RCONNECT_")
    #' fig$configure(env_prefix = "")
    #' fig$configure() # has no effect
    configure = function(env_prefix) {
      if (!missing(env_prefix)) {
        stopifnot(
          is.character(env_prefix),
          !is.na(env_prefix),
          length(env_prefix) == 1
        )
        private$add_env_prefix <- function(key) paste0(env_prefix, key)
      }
      invisible(self)
    },

    #' @description Delete Stored Values
    #' @param ... Keys to be deleted.
    #' @return Reference to self. Other methods can be chained after this one.
    #' @examples
    #' fig <- Fig$new()
    #' fig$store_many("foo" = 1, "bar" = 2, "baz" = 3)
    #' fig$delete("foo")
    #' fig$delete("bar", "baz")
    #' fig$get_many("foo", "bar", "baz") # == list(NULL, NULL, NULL)
    delete = function(...) {
      rm(list = c(...), envir = private$items)
      invisible(self)
    },

    #' @description Delete All Stored Values
    #' @return Reference to self. Other methods can be chained after this one.
    #' @examples
    #' fig <- Fig$new()
    #' fig$store_many("foo" = 1, "bar" = 2, "baz" = 3)
    #' fig$delete_all()
    #' fig$get_many("foo", "bar", "baz") # == list(NULL, NULL, NULL)
    delete_all = function() {
      private$items <- new.env()
      invisible(self)
    },

    #' @description Retrieve a Stored Value
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
    #' @param key A key to retrieve a value for.
    #' @param split A logical determining whether dots in `key` are treated
    #' specially or as is. See Details section.
    #' @return A value associated with provided `key`.
    #' @examples
    #' fig <- Fig$new()
    #' fig$store("foo", 1)
    #' fig$get("foo")
    #'
    #' fig$store("bar", list(baz = 2))
    #' fig$get("bar.baz")
    #'
    #' fig$store("bar.baz", 3, split = FALSE)
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

    #' @description Retrieve Any Number of Stored Values
    #' @param ... Keys to retrieve values for.
    #' @param .split A logical determining whether dots in `key` are treated
    #' specially or as is. See Details section in `get()`.
    #' @return An unnamed list of values associated with keys provided in `...`.
    get_many = function(..., .split = getOption("fig.split", TRUE)) {
      lapply(list(...), self$get, split = .split)
    },

    #' @description Retrieve All Stored Values
    #' @return An unnamed list of all stored values.
    get_all = function() {
      as.list(private$items)
    },

    #' @description Store a Value
    #' @details Fig treats dots in `key` as nest level delimiters. Therefore,
    #' `fig$store("foo.bar", 1)` is equivalent to `fig$store("foo", list(bar =
    #' 1)`. This behavior can be disabled either by setting `options(fig.split
    #' = FALSE)` or by providing `split = FALSE` argument.
    #' @param key A key to store a value for.
    #' @param value A value to be stored.
    #' @param split A logical determining whether dots in `key` are treated
    #' specially or as is. See Details section.
    #' @return Reference to self. Other methods can be chained after this one.
    #' @examples
    #' fig <- Fig$new()
    #' fig$store("foo", 1)
    #' fig$store("bar", 123)$store("baz", list(1, 2, 3))
    #' fig$store("x.y", "a", FALSE)
    store = function(key, value, split = getOption("fig.split", TRUE)) {
      if (isTRUE(split)) {
        private$insert_value(key, value)
      } else {
        private$items[[key]] <- value
      }
      invisible(self)
    },

    #' @description Store a List's Contents
    #' @param l (named list) Names are used as keys for storing their values.
    #' @param split A logical determining whether dots in `key` are treated
    #' specially or as is. See Details section in `store()`.
    #' @return Reference to self. Other methods can be chained after this one.
    #' @examples
    #' fig <- fig$New()
    #' fig$store_list(list(foo = 1, bar = 2))
    #' fig$store_list(list(foo = 123, baz = "abc"), split = TRUE)
    store_list = function(l, split = getOption("fig.split", TRUE)) {
      keys <- names(l)
      stopifnot(
        !is.null(keys),
        all(keys != ""),
        length(unique(keys)) == length(keys)
      )
      for (key in keys) {
        self$store(key, l[[key]], split)
      }
      invisible(self)
    },

    #' @description Set Any Number of Values
    #' @param ... Named arguments. Names are used as keys for storing argument
    #' values.
    #' @param .split A logical determining whether dots in `key` are treated
    #' specially or as is. See Details section in `store()`.
    #' @return Reference to self. Other methods can be chained after this one.
    #' @examples
    #' fig <- Fig$new()
    #' fig$store_many("foo" = 1, "bar" = 2)
    #' fig$store_many("foo.bar.baz" = 1, .split = TRUE)
    #' fig$store_many("foo" = "a", "baz" = 123, .split = FALSE)
    store_many = function(..., .split = getOption("fig.split", TRUE)) {
      self$store_list(list(...), .split)
    }
  ),
  private = list(
    add_env_prefix = NULL,
    items = new.env(),
    insert_value = function(key, value) {
      keys <- strsplit(key, ".", TRUE)[[1]]
      n_keys <- length(keys)
      # Descend recursively through keys. Insert value at the end of this chain.
      descend <- function(l, lvl = 1) {
        key <- keys[[lvl]]
        if (lvl < n_keys) {
          l[[key]] <- descend(l[[key]], lvl + 1)
        } else {
          # In case key has to be added next to a single value. With check
          # against private$items to not convert it.
          if (!identical(private$items, l) && !is.list(l)) {
            l <- as.list(l)
          }
          l[[key]] <- value
        }
        l
      }
      private$items <- descend(private$items)
    },
    traverse_items = function(key) {
      value <- private$items
      for (key_part in strsplit(key, ".", TRUE)[[1]]) {
        value <- value[[key_part]]
      }
      value
    }
  )
)

fig <- Fig$new()

#' @param env_prefix (character) A prefix to be prepended to a key before
#' system environment lookup. Pass an empty string to reset.
#' @return Reference to self. Other methods can be chained after this one.
#' @rdname Fig
#' @export
fig_configure <- function(env_prefix) {
  fig$configure(env_prefix)
}

#' @param ... Keys to be deleted.
#' @return Reference to self. Other methods can be chained after this one.
#' @rdname Fig
#' @export
fig_delete <- function(...) {
  fig$delete(...)
}

#' @return Reference to self. Other methods can be chained after this one.
#' @rdname Fig
#' @export
fig_delete_all <- function() {
  fig$delete_all()
}

#' @param key A key to retrieve a value for.
#' @param split A logical determining whether dots in `key` are treated
#' specially or as is. See Details section.
#' @return A value associated with provided `key`.
#' @rdname Fig
#' @export
fig_get <- function(key, split = getOption("fig.split", TRUE)) {
  fig$get(key, split)
}

#' @param ... Keys to retrieve values for.
#' @param .split A logical determining whether dots in `key` are treated
#' specially or as is. See Details section in `get()`.
#' @return An unnamed list of values associated with keys provided in `...`.
#' @rdname Fig
#' @export
fig_get_many <- function(..., .split = getOption("fig.split", TRUE)) {
  fig$get_many(..., .split = .split)
}

#' @return An unnamed list of all stored values.
#' @rdname Fig
#' @export
fig_get_all <- function() {
  fig$get_all
}

#' @param key A key to store a value for.
#' @param value A value to be stored.
#' @param split A logical determining whether dots in `key` are treated
#' specially or as is. See Details section.
#' @return Reference to self. Other methods can be chained after this one.
#' @rdname Fig
#' @export
fig_store <- function(key, value, split = getOption("fig.split", TRUE)) {
  fig$store(key, value, split)
}

#' @param l (named list) Names are used as keys for storing their values.
#' @param split A logical determining whether dots in `key` are treated
#' specially or as is. See Details section in `store()`.
#' @return Reference to self. Other methods can be chained after this one.
#' @rdname Fig
#' @export
fig_store_list <- function(l, split = getOption("fig.split", TRUE)) {
  fig$store_list(l, split)
}

#' @param ... Named arguments. Names are used as keys for storing argument
#' values.
#' @param .split A logical determining whether dots in `key` are treated
#' specially or as is. See Details section in `store()`.
#' @return Reference to self. Other methods can be chained after this one.
#' @rdname Fig
#' @export
fig_store_many <- function(..., .split = getOption("fig.split", TRUE)) {
  fig$store_many(..., .split = .split)
}

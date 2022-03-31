#' @title Fig for Your Config
#'
#' @description `Fig` class is a main driver of this package. For usage details
#' refer to `Fig` class methods documentation.
#'
#' Fig provides a set of exported functions. This makes `Fig` class instance
#' creation optional, and makes the package itself mimic being a class instance.
#' Those functions are wrappers on an internal `Fig` object.
#'
#' @import R6
#' @export
Fig <- R6::R6Class( # nolint
  classname = "Fig",
  public = list(
    #' @description Create a New Fig Instance
    #' @details Fig treats character provided in `split_on` as key nest level
    #' delimiter. Therefore, `split_on` set to `"."` (default value)
    #' `fig$get("foo.bar")` is equivalent to `fig$get("foo")$bar`. Similarly
    #' fig$set("foo.bar", 1) is equivalent to `fig$set("foo", list(bar = 1))`.
    #' This behavior can be disabled either by passing an empty string either to
    #' `new()` during `Fig` instance creation or to `configure()` function to
    #' modify an existing instance.
    #' @param env_prefix (character) A prefix to be prepended to a key before
    #' system environment lookup.
    #' @param split_on (character) A value to split keys on. See Details
    #' section. Providing an empty string disables this behavior.
    #' @return New instance of `Fig`.
    #' @examples
    #' fig <- Fig$new()
    #' fig <- Fig$new(env_prefix = "RCONNECT_")
    initialize = function(env_prefix = "", split_on = ".") {
      self$configure(env_prefix = env_prefix, split_on = split_on)
      private$storage <- new.env()
    },

    #' @description Configure a Fig Instance
    #' @details Unset arguments do not change configuration.
    #' @param env_prefix (character) A prefix to be prepended to a key before
    #' system environment lookup. Pass an empty string to reset.
    #' @param split_on (character) A value to split keys on. See Details
    #' section in `new()`. Providing an empty string disables this behavior.
    #' @return Reference to self. Other methods can be chained after this one.
    #' @examples
    #' fig <- Fig$new(env_prefix = "RCONNECT_")
    #' fig$configure(env_prefix = "foo_")
    #' fig$configure(split_on = "")
    #' fig$configure() # has no effect
    configure = function(env_prefix, split_on) {
      if (!missing(env_prefix)) {
        stopifnot(
          is.character(env_prefix),
          !is.na(env_prefix),
          length(env_prefix) == 1
        )
        private$add_env_prefix <- function(key) paste0(env_prefix, key)
        private$config$env_prefix <- env_prefix
      }
      if (!missing(split_on)) {
        stopifnot(
          is.character(split_on),
          !is.na(split_on),
          length(split_on) == 1
        )
        private$config$split_on <- split_on
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
      rm(list = c(...), envir = private$storage)
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
      private$storage <- new.env()
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
    #' @param key A key to retrieve a value for.
    #' @return A value associated with provided `key`.
    #' @examples
    #' fig <- Fig$new()
    #' fig$store("foo", 1)
    #' fig$get("foo")
    #'
    #' fig$store("bar", list(baz = 2))
    #' fig$get("bar.baz")
    #'
    #' fig$configure(split_on = "")
    #' fig$get("bar.baz") # == NULL
    get = function(key) {
      stopifnot(length(key) == 1, !is.na(key), nchar(key) > 0)
      env_key <- sub(".", "_", key, fixed = TRUE)
      value <- Sys.getenv(private$add_env_prefix(env_key), NA)
      if (!is.na(value)) {
        return(value)
      }
      split_on <- private$config$split_on
      if (nchar(split_on) > 0) {
        private$traverse_storage(key, split_on)
      } else {
        private$storage[[key]]
      }
    },

    #' @description Retrieve Any Number of Stored Values
    #' @details See `get()` Details section.
    #' @param ... Keys to retrieve values for.
    #' @return An unnamed list of values associated with keys provided in `...`.
    #' @examples
    #' fig <- Fig$new()
    #' fig$store_many(foo =  1, bar = 2, baz = 3)
    #' fig$get_many("foo", "bar")
    get_many = function(...) {
      lapply(list(...), self$get)
    },

    #' @description Retrieve All Stored Values
    #' @details See `get()` Details section.
    #' @return An unnamed list of all stored values.
    #' @examples
    #' fig <- Fig$new()
    #' fig$store_many(foo =  1, bar = 2, baz = 3)
    #' fig$get_all()
    get_all = function() {
      as.list(private$storage)
    },

    #' @description Store a Value
    #' @param key A key to store a value for.
    #' @param value A value to be stored.
    #' @return Reference to self. Other methods can be chained after this one.
    #' @examples
    #' fig <- Fig$new()
    #' fig$store("foo", 1)
    #' fig$store("bar", 123)$store("baz", list(1, 2, 3))
    #' fig$store("x.y", "a")
    store = function(key, value) {
      split_on <- private$config$split_on
      if (nchar(split_on) > 0) {
        private$insert_value(key, value, split_on)
      } else {
        private$storage[[key]] <- value
      }
      invisible(self)
    },

    #' @description Store a List's Contents
    #' @param l (named list) Names are used as keys for storing their values.
    #' @return Reference to self. Other methods can be chained after this one.
    #' @examples
    #' fig <- Fig$new()
    #' fig$store_list(list(foo = 123, bar = "abc"))
    store_list = function(l) {
      keys <- names(l)
      stopifnot(
        !is.null(keys),
        all(keys != ""),
        length(unique(keys)) == length(keys)
      )
      for (key in keys) {
        self$store(key, l[[key]])
      }
      invisible(self)
    },

    #' @description Set Any Number of Values
    #' @param ... Named arguments. Names are used as keys for storing argument
    #' values.
    #' @return Reference to self. Other methods can be chained after this one.
    #' @examples
    #' fig <- Fig$new()
    #' fig$store_many("foo" = 1, "bar" = 2)
    #' fig$store_many("foo.bar.baz" = 1)
    #' fig$store_many("foo" = "a", "baz" = 123)
    store_many = function(...) {
      self$store_list(list(...))
    }
  ),
  private = list(
    add_env_prefix = NULL,
    config = list(),
    storage = NULL,
    insert_value = function(key, value, split_on) {
      keys <- strsplit(key, split_on, TRUE)[[1]]
      n_keys <- length(keys)
      # Descend recursively through keys. Insert value at the end of this chain.
      descend <- function(l, lvl = 1) {
        key <- keys[[lvl]]
        if (lvl < n_keys) {
          # In case the we need to descend into a key that holds a value that is
          # not a list (e.g. a numeric). Check against private$storage to not
          # convert it.
          if (!identical(private$storage, l) && !is.list(l)) {
            l <- list()
          }
          l[[key]] <- descend(l[[key]], lvl + 1)
        } else {
          # In case the key has to be added next to a single value. Check
          # against private$storage to not convert it.
          if (!identical(private$storage, l) && !is.list(l)) {
            l <- as.list(l)
          }
          l[[key]] <- value
        }
        l
      }
      private$storage <- descend(private$storage)
    },
    traverse_storage = function(key, split_on) {
      value <- private$storage
      for (key_part in strsplit(key, split_on, TRUE)[[1]]) {
        value <- value[[key_part]]
      }
      value
    }
  )
)

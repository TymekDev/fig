#' @title Create a Fig to Store Config
#'
#' @description `Fig` class is a main driver of this package. For usage details
#' refer to `Fig` methods documentation.
#'
#' Fig provides a set of exported functions. This makes `Fig` class instance
#' creation optional, and makes the package mimic being a class instance. Those
#' functions are simple wrappers on an internal fig object and are prefixed to
#' avoid masking.
#'
#' @export
Fig <- R6::R6Class(
  classname = "Fig",
  public = list(
    #' @description Create a new Fig instance
    initialize = function() {
      private$items <- new.env()
    },

    #' @description Get a stored value
    #' @param key A key value to retrieve stored value for.
    get = function(key) {
      private$items[[key]]
    },

    #' @description Store a value
    #' @param key A key value to store a value for.
    #' @param value A value to be stored.
    set = function(key, value) {
      private$items[[key]] <- value
      invisible(self)
    }
  ),
  private = list(
    items = NULL
  )
)

fig <- Fig$new()

#' @param key A key value to retrieve stored value for.
#' @rdname Fig
#' @export
fig_get <- function(key) fig$get(key)

#' @param key A key value to store a value for.
#' @param value A value to be stored.
#' @rdname Fig
#' @export
fig_set <- function(key, value) fig$set(key, value)

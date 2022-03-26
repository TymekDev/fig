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

    #' @description Delete a stored value
    #' @param key A key to delete its corresponding value.
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
    #' @param key A key to retrieve its corresponding value
    get = function(key) {
      value <- Sys.getenv(key, NA)
      if (!is.na(value)) {
        return(value)
      }
      private$items[[key]]
    },

    #' @description Store a value
    #' @param key A key to store a value for.
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

#' @param key A key to delete its corresponding value.
#' @rdname Fig
#' @export
fig_delete <- function(key) global_fig()$delete(key)

#' @param key A key to retrieve its corresponding value
#' @rdname Fig
#' @export
fig_get <- function(key) global_fig()$get(key)

#' @param key A key to store a value for.
#' @param value A value to be stored.
#' @rdname Fig
#' @export
fig_set <- function(key, value) global_fig()$set(key, value)

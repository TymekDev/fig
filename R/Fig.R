#' @title Create a Fig to Store Config
#'
#' @description Fig is the main driver in fig package.
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

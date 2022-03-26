#' @title Purge Values Stored in Global Fig
#' @description This function replaces internal fig object with a new instance
#' of `Fig` class, therefore giving a `fig_*` functions a fresh start.
#' @export
purge <- function() assign("fig", Fig$new(), envir = global_fig_env)
global_fig <- function() global_fig_env$fig


# NOTE: variables cannot have its value changed inside a package. However,
# environments can have its contents modified.
global_fig_env <- new.env()
purge() # Initialize global_fig

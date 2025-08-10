
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check if all elements are named
#'
#' @param x object e.g. list, vector
#' @return TRUE if all elements are named, otherwise FALSE
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all_named <- function(x) {
  named <- nzchar(names(x))
  length(x) == 0L || (!is.null(named) && length(named) > 0 && all(named))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simpler? version of modifyList
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# modify_list <- function (old, new) {
#   for (i in names(new)) old[[i]] <- new[[i]]
#   old
# }

is_missing_arg <- function (x) identical(x, quote(expr = ))

find_args <- function (...) {
  env  <- parent.frame()
  args <- names(formals(sys.function(sys.parent(1))))
  vals <- mget(args, envir = env)
  vals <- vals[!vapply(vals, is_missing_arg, logical(1))]
  # modify_list(vals, list(..., ... = NULL))
  
  res <- modifyList(vals, list(...), keep.null = TRUE)
  res['...'] <- NULL
  res
}


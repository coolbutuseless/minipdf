
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

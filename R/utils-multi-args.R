
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Stolen from ggplot. used with themes to boil down a long
# argument list to just those values which were used in the function call
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
modify_list <- function (old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}

is_missing_arg <- function (x) identical(x, quote(expr = ))

find_args <- function (...) {
  env  <- parent.frame()
  args <- names(formals(sys.function(sys.parent(1))))
  vals <- mget(args, envir = env)
  vals <- vals[!vapply(vals, is_missing_arg, logical(1))]
  modify_list(vals, list(..., ... = NULL))
}

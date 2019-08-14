


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert public methods in an R6 to wrapper functions
#'
#' This function is inspired by the \code{R62S3} package.
#'
#' @param r6class the R6Class object
#' @param envir the environment in which to assign the wrapper functions
#' @param object_name the argument name as the object should appear in the functions
#' @param include,exclude names of methods to include/exclude
#' @param rename named vector to define name of function if it should be
#'        different from the method name e.g. \code{c(func_name = "method_name")}
#' @param silent vector of names of R6 methods which should return invisible result.
#'        default: NULL
#'
#' @importFrom stats setNames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
r6_to_funcs <- function (r6class, envir = parent.env(),
                         object_name = 'r6object',
                         include = NULL, exclude = c('clone', 'initialize'), rename = NULL,
                         silent = NULL) {

  stopifnot(inherits(r6class, "R6ClassGenerator"))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ask the class for all its public methods
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  methods <- r6class$public_methods

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # User may explicitly specify the methods to include/exclude from being
  # turned into a function
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(include)) { methods <- methods[ names(methods) %in% include] }
  if (!is.null(exclude)) { methods <- methods[!names(methods) %in% exclude] }


  for (i in seq_along(methods)) {
    method_name <- names(methods)[[i]]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Have a different function name from the method name?
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (method_name %in% rename) {
      func_name <- names(which(method_name == rename))[1]
    } else {
      func_name <- method_name
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Build an empty function with the right arguments
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    func          <- function() {}
    x             <- alist(x = )
    names(x)      <- object_name
    formals(func) <- c(x, formals(methods[[i]]))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Function wrapper should
    #  - take the args
    #  - remove the r6object from the args list
    #  - call the approprirate method on the object
    #  - return invisible result if requested
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    body(func) = bquote({
      args <- setdiff(names(as.list(match.call())[-1]), .(object_name))
      args <- setNames(args, args)
      args <- lapply(args, as.name)
      res  <- do.call(.(as.name(object_name))[[.(method_name)]], args)
      if (.(method_name %in% silent)) invisible(res) else (res)
    })

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Put the new function in the disired namespace
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    assign(func_name, func, envir = envir)
  }
}



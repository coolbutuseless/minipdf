

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a PDF Dict object 
#' 
#' PDF Dicts are just lists of "name = value" mappings.
#' Any object with a value of NULL is removed.
#' 
#' @param ... named arguments
#' @return 'pdf_dict' object
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_dict <- function(...) {
  dict <- list(...)
  if (!all_named(dict)) {
    print(dict)
    stop("pdf_dict(): Not all named")
  }
  
  dict <- Filter(Negate(is.null), dict)  
  
  class(dict) <- 'pdf_dict'
  dict
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Render \code{pdf_dict} as character string
#' 
#' @param x pdf_dict
#' @param depth print depth. Default: 0.  Used to control indentation
#' @param ... ignored
#' @return None.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.pdf_dict <- function(x, depth = 0, ...) {
  indent1 <- paste0(rep("  ", depth), collapse = "")
  indent2 <- paste0(rep("  ", depth + 1), collapse = "")
  nms   <- names(x)
  elems <- unname(x)
  
  s <- lapply(seq_along(nms), \(i) {
    if (is_dict(elems[[i]])) {
      # Recurse
      glue::glue("{indent2}/{nms[i]}\n{as.character(elems[[i]], depth = depth + 1)}")
    } else {
      glue::glue("{indent2}/{nms[i]} {as.character(elems[[i]])}")
    }
  })
  
  # indent and collapse the members of the dict
  s <- paste(s, collapse = "\n")
  glue::glue("{indent1}<<\n{s}\n{indent1}>>")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print a pdf_dict
#' @param x pdf_dict
#' @param ... ignored
#' @return None.
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.pdf_dict <- function(x, ...) {
  cat("<dict>\n")
  cat(as.character(x), "\n")
  invisible(x)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check an object is a dict
#' 
#' @param x obj
#' @return logical
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_dict <- function(x) {
  isTRUE(inherits(x, 'pdf_dict'))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Assert an object is a dict
#' 
#' @param x obj
#' @return None
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
assert_dict <- function(x) {
  stopifnot(is_dict(x))
}



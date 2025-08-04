

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an empty shell for the PDF intermediate format
#' 
#' @param ... options
#' @return List with attributes. List items are PDF objects.  Attributes
#'         are PDF settings
#' @examples
#' create_pif()
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pif <- function(...) {
  pif <- list()
  atts <- list(...) 
  if (length(atts) > 0) {
    stopifnot(all_named(atts))
    attributes(pif) <- atts
  }
  class(pif) <- 'pif'
  
  pif
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a PDF Dict object in pif format
#' @param ... named arguments
#' @return 'pif_dict' object
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pif_dict <- function(...) {
  dict <- list(...)
  if (!all_named(dict)) {
    print(dict)
    stop("Not all named")
  }
  class(dict) <- 'dict'
  dict
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print a pif_dict. Can be nested
#' @param x pif_dict
#' @param depth print depth. Default: 0.  Used to control indentation
#' @param ... ignored
#' @return None.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.dict <- function(x, depth = 0, ...) {
  
  indent <- paste0(rep(" ", depth), collapse = "")

  for (i in seq_along(x)) {
    if (is_dict(x[[i]])) {
      cat(indent, names(x)[i], ": ", "\n", sep = "")
      print.dict(x[[i]], depth = depth + 1)
    } else {
      cat(indent, names(x)[i], ": ", x[[i]], "\n", sep = "")
    }
  }
  invisible()
}


if (FALSE) {
  zz <- pif_dict(type = "Hello", greg = pif_dict(x = "next"))
  zz
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check an object is a dict
#' 
#' @param x obj
#' @return logical
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_dict <- function(x) {
  isTRUE(inherits(x, 'dict'))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Assert an object is a dict
#' 
#' @param x obj
#' @return None
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
assert_dict <- function(x) {
  stopifnot(inherits(x, 'dict'))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Append a 'dict' to the 'pif'
#' 
#' @param pif pif
#' @param dict dict
#' @return pif
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pif_append <- function(pif, dict) {
  append(pif, list(dict))
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print a 'pif' object
#' 
#' @param x pif object
#' @param ... ignored
#' @return None
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.pif <- function(x, ...) {
  cat("<pif> with", length(x), "PDF objects\n")
  invisible(x)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write from the intermediate format to an actual PDF file
#' @param pif pif object as created by \code{\link{create_pif}()}
#' @param filename output filename
#' @return None
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_pdf <- function(pif, filename) {
  
  # write header
  # write objects
  # write xref table
  
  invisible(pif)
}



if (FALSE) {
  fontname <- 'Helvetica'
  width <- 400
  height <- 300
  pif <- create_pif()
  pif <- pif_append(pif, pif_dict(Type = '/Catalog', Pages = "2 0 R"))
  pif <- pif_append(pif, pif_dict(Type = '/Pages', Kids = "[3 0 R]", Count = 1))
  pif <- pif_append(
    pif, 
    pif_dict(
    Type      = '/Page',
    Parent    = "2 0 R",
    Resources = "4 0 R",
    MediaBox  = glue::glue("[0 0 {width} {height}]"),
    Contents  = "[6 0 R]"
    )
  )
  pif <- pif_append(
    pif, 
    pif_dict(
    Font      = pif_dict(F1 = "5 0 R"),
    ExtGState = pif_dict(GS11 = pif_dict(ca = 1, CA = 1)))
  )
  pif <- pif_append(pif, pif_dict(Type = '/Font', Subtype = "/Type1", BaseFont = paste0("/", fontname)))
  
  pif
  
  
  pif_dict(x = "Hello", y = pif_dict(x = "next")) |> str()
  
  
  
}












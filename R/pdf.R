

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an empty shell for the PDF intermediate format
#' 
#' @param ... options
#' @return List with attributes. List items are PDF objects.  Attributes
#'         are PDF settings
#' @examples
#' create_pdf()
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pdf <- function(...) {
  doc <- list()
  atts <- list(...) 
  if (length(atts) > 0) {
    stopifnot(all_named(atts))
    attributes(doc) <- atts
  }
  class(doc) <- 'pdf_doc'
  
  doc
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a PDF Dict object 
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
  class(dict) <- 'pdf_dict'
  dict
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print a pdf Can be nested
#' @param x pdf_dict
#' @param depth print depth. Default: 0.  Used to control indentation
#' @param ... ignored
#' @return None.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.pdf_dict <- function(x, depth = 0, ...) {
  
  indent <- paste0(rep("  ", depth), collapse = "")

  for (i in seq_along(x)) {
    if (is_dict(x[[i]])) {
      cat(indent, names(x)[i], ": ", "\n", sep = "")
      print.pdf_dict(x[[i]], depth = depth + 1)
    } else {
      cat(indent, names(x)[i], ": ", x[[i]], "\n", sep = "")
    }
  }
  invisible()
}


if (FALSE) {
  zz <- pdf_dict(type = "Hello", greg = pdf_dict(x = "next"))
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Append a 'dict' to the 'pdf'
#' 
#' @param doc pdf_doc
#' @param dict pdf_dict
#' @return pdf
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_append <- function(doc, dict) {
  append(doc, list(dict))
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print a 'pdf' object
#' 
#' @param x pdf object
#' @param ... ignored
#' @return None
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.pdf_doc <- function(x, ...) {
  cat("<pdf doc> with", length(x), "PDF objects\n")
  invisible(x)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a pdf stream object
#' @param ... named arguments
#' @return 'stream' object
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_stream <- function(type, ...) {
  stream <- list(...)
  if (!all_named(stream)) {
    print(stream)
    stop("Not all named")
  }
  class(stream) <- 'stream'
  attr(stream, 'type') <- type
  stream
}

is_stream <- function(x) {
  isTRUE(inherits(x, 'pdf_stream'))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a line in pdf
#' @param x1,y1,x2,y2 endpoints
#' @return stream object
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_line <- function(x1, y1, x2, y2) {
  pdf_stream(type = 'line', x1 = x1, y1 = y1, x2 = x2, y2 = y2)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert stream to character
#' 
#' @param x pdf_stream object
#' @param ... ignored
#' @return character string
#' @importFrom glue glue glue_data
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.pdf_stream <- function(x, ...) {
  type <- attr(x, 'type', exact = TRUE)
  if (type == 'line') {
    glue::glue_data(x, "{x1} {y1} m {x2} {y2} l s")
  } else {
    stop("Unknown stream: ", deparse1(class(x)))
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' print stream
#' @param x pdf_stream
#' @param ... ignored
#' @return None
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.pdf_stream <- function(x, ...) {
  type <- attr(x, 'type', exact = TRUE)
  cat(type, ": ", dQuote(as.character(x, ...), FALSE), "\n", sep = "")
  invisible(x)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert stream to PDF object representation
#' This includes wrapping it in stream/endstream, counting characters etc
#' @param stream stream
#' @param idx idx
#' @importFrom glue glue
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stream_to_pdf <- function(stream, idx) {
  str         <- as.character(stream)
  nchars      <- nchar(str)
  this_stream <- glue::glue("stream\n{str}\nendstream")
  this_length <- glue::glue("<< /Length {nchars} >>")
  res <- glue::glue("{idx} 0 obj\n{this_length}\n{this_stream}\nendobj")
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' export pdf
#'
#' @param doc pdf_doc
#' @return string
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_export <- function(doc) {
  
}



if (FALSE) {
  fontname <- 'Helvetica'
  width <- 400
  height <- 300
  doc <- create_pdf()
  doc <- pif_append(doc, pif_dict(Type = '/Catalog', Pages = "2 0 R"))
  doc <- pif_append(doc, pif_dict(Type = '/Pages', Kids = "[3 0 R]", Count = 1))
  doc <- pif_append(
    doc, 
    pif_dict(
    Type      = '/Page',
    Parent    = "2 0 R",
    Resources = "4 0 R",
    MediaBox  = glue::glue("[0 0 {width} {height}]"),
    Contents  = "[6 0 R]"
    )
  )
  doc <- pif_append(
    doc, 
    pif_dict(
    Font      = pif_dict(F1 = "5 0 R"),
    ExtGState = pif_dict(GS11 = pif_dict(ca = 1, CA = 1)))
  )
  doc <- pif_append(doc, pif_dict(Type = '/Font', Subtype = "/Type1", BaseFont = paste0("/", fontname)))
  
  doc
  
  
  pdf_dict(x = "Hello", y = pdf_dict(x = "next"))
  
  
  
}












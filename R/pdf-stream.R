

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
  class(stream) <- 'pdf_stream'
  attr(stream, 'type') <- type
  stream
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check if an object is a 'pdf_stream'
#' @param x object to test
#' @return logical
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_stream <- function(x) {
  isTRUE(inherits(x, 'pdf_stream'))
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
  gp <- paste(c(
    "0.0 G",
    "5 w", 
    "0 0 0 rg", 
    "1 0 0 RG", 
    ""
  ), collapse = "\n")
  if (type == 'line') {
    s <- glue::glue_data(x, "{gp}{x1} {y1} m {x2} {y2} l s")
  } else if (type == 'rect') {
    # paint types: 
    #  - s close & stroke path
    #  - S stroke path
    #  - f  fill (winding number)
    #  - f* fill (even-odd)
    #  - B  fill & stroke (winding)
    #  - B* fill & stroke (even-odd)
    #  - b  close, fill & stroke (winding)
    #  - b* close, fill & stroke (even-odd)
    #  - n end path without stroke or fill. used to define clipping path
    s <- glue::glue_data(x, "{gp}{x} {y} {width} {height} re B")
  } else {
    stop("Unknown stream: ", deparse1(class(x)))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Always push/pop the local graphics state
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  s <- paste('q', s, 'Q', sep = "\n")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # A stream is always prefixed with a dict giving its length
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  len <- nchar(s)
  len <- as.character(pdf_dict(Length = len))
  s <- glue::glue(
    "{len}
    stream
    {s}
    endstream"
  )
  
  
  s
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' print stream
#' @param x pdf_stream
#' @param ... ignored
#' @return None
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.pdf_stream <- function(x, ...) {
  cat("<stream: ")
  type <- attr(x, 'type', exact = TRUE)
  cat(type, ">\n", sep = "")
  cat(as.character(x, ...), "\n", sep = "")
  invisible(x)
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
#' Create a rect
#' @param x,y position
#' @param width,height size
#' @return stream object
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_rect <- function(x, y, width, height) {
  pdf_stream(type = 'rect', x = x, y = y, width = width, height = height)
}














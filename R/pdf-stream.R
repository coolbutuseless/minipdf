

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
  
  type  <- attr(x, 'type', exact = TRUE)
  paint <- gp_to_closed_paint_op(x$gp)
  
  if (type == 'line') {
    s <- glue::glue_data(x, "{x1} {y1} m {x2} {y2} l s")
  } else if (type == 'rect') {
    s <- glue::glue_data(x, "{x} {y} {width} {height} re {paint}")
  } else {
    stop("Unknown stream: ", deparse1(class(x)))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add graphics state operators
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gs <- gp_to_gs_operators(x$gp)
  s  <- paste(gs, s, sep = "\n")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add reference to graphics state dict
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  s <- paste("/GS11 gs", s, sep = "\n")
  
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
#' @param gp A named list \code{gp} object created by \code{\link{pgpar}()}
#' @param ... further arguments to be added to \code{gp}
#' @return stream object
#' @export
#' @importFrom utils modifyList
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_line <- function(x1, y1, x2, y2, ..., gp = pgpar()) {
  gp <- modifyList(gp, list(...))
  pdf_stream(
    type = 'line', 
    gp   = gp,
    x1 = x1, y1 = y1, x2 = x2, y2 = y2, gp = gp
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a rect
#' @param x,y position
#' @param width,height size
#' @inheritParams pdf_line
#' @return stream object
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_rect <- function(x, y, width, height, ..., gp = pgpar()) {
  gp <- modifyList(gp, list(...))
  pdf_stream(
    type = 'rect', 
    gp   = gp,
    x = x, y = y, width = width, height = height
  )
}









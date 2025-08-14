

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a translation specification (for use as \code{tf} argument)
#' 
#' @param x,y translation 
#' @return translation specification
#' @examples
#' doc <- create_pdf() |>
#'    pdf_text(text = "hello", x = 0, y = 0, tf = tf_translate(x = 10, y = 10))
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tf_translate <- function(x, y) {
  
  stopifnot(
    is_numeric_1(x),
    is_numeric_1(y)
  )
  
  structure(
    list(x = x, y = y),
    class = c('pdf_transform', 'pdf_translate')
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert scale/rotate/translate specification to a PDF transformation string
#' 
#' @param x transform specification
#' @param ... ignored
#' @return String representing a PDF transformation matrix 'cm' operation
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.pdf_translate <- function(x, ...) {
  glue::glue_data(x, "1 0 0 1 {x} {y} cm")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a rotation specification (for use as \code{tf} argument)
#' 
#' @param rads rotation angle in radians
#' @param x,y location to rotate around
#' @return rotation specification
#' @examples
#' doc <- create_pdf() |>
#'    pdf_text(text = "hello", x = 0, y = 0, tf = tf_rotate(rads = pi))
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tf_rotate <- function(rads, x = 0, y = 0) {
  
  stopifnot(
    is_numeric_1(rads),
    is_numeric_1(x),
    is_numeric_1(y)
  )
  
  structure(
    list(rads = rads, x = x, y = y),
    class = c('pdf_transform', 'pdf_rotate')
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname as.character.pdf_translate
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.pdf_rotate <- function(x, ...) {
  
  cosQ <- round(cos(x$rads), 5)
  sinQ <- round(sin(x$rads), 5)
  
  if (x$x == 0 && x$y == 0) {
    rot <- glue::glue("{cosQ} {sinQ} {-sinQ} {cosQ} 0 0 cm")
  } else {
    rot <- glue::glue(
      "1 0 0 1 {x$x} {x$y} cm",
      "{cosQ} {sinQ} {-sinQ} {cosQ} 0 0 cm",
      "1 0 0 1 {-x$x} {-x$y} cm",
      .sep = "\n"
    )
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a scaling specification (for use as \code{tf} argument)
#' 
#' @param x,y scale amount in each direction. If 'y' value is not specified
#'        it is made the same as the 'x' value
#' @return scale transform specification
#' @examples
#' doc <- create_pdf() |>
#'    pdf_text(text = "hello", x = 0, y = 0, tf = tf_scale(x = 10))
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tf_scale <- function(x, y = x) {
  
  stopifnot(
    is_numeric_1(x),
    is_numeric_1(y)
  )
  
  structure(
    list(x = x, y = y),
    class = c('pdf_transform', 'pdf_scale')
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname as.character.pdf_translate
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.pdf_scale <- function(x, ...) {
  glue::glue_data(x, "{x} 0 0 {y} 0 0 cm")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname as.character.pdf_translate
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.pdf_transform_list <- function(x, ...) {
  if (length(x) == 0) {
    character(0)
  } else {
    res <- vapply(x, as.character, character(1))
    paste(res, collapse = "\n")
  }
}






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Modify global transformation matrix with additional translation
#' 
#' Global transformations are cumulative, and these is no operation to reset
#' the global transformation.
#"
#' For local transformations use the \code{tf} argument for individual objects.
#' 
#' @inheritParams pdf_line
#' @inheritParams tf_translate
#' @return \code{pdf_doc}
#' @examples
#' doc <- create_pdf() |>
#'    pdf_translate(x = 10, y = 10)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_translate <- function(doc, x, y) {
  
  obj <- pdf_stream(
    type = 'pdf_transform', 
    gp   = pgpar(),
    tf   = NULL,
    transform = tf_translate(x, y)
  )
  
  pdf_add(doc, obj)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Modify global transformation matrix with additional rotation
#' 
#' Global transformations are cumulative, and these is no operation to reset
#' the global transformation.
#"
#' For local transformations use the \code{tf} argument for individual objects.
#' 
#' @inheritParams pdf_line
#' @inheritParams tf_rotate
#' @return \code{pdf_doc}
#' @examples
#' doc <- create_pdf() |>
#'    pdf_rotate(rads = pi)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_rotate <- function(doc, rads, x = 0, y = 0) {
  
  obj <- pdf_stream(
    type = 'pdf_transform', 
    gp   = pgpar(),
    tf   = NULL,
    transform = tf_rotate(rads, x, y)
  )
  
  pdf_add(doc, obj)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Modify global transformation matrix with additional scaling
#' 
#' Global transformations are cumulative, and these is no operation to reset
#' the global transformation.
#"
#' For local transformations use the \code{tf} argument for individual objects.
#' 
#' @inheritParams pdf_line
#' @inheritParams tf_scale
#' @return \code{pdf_doc}
#' @examples
#' doc <- create_pdf() |>
#'    pdf_scale(x = 10)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_scale <- function(doc, x, y = x) {
  
  obj <- pdf_stream(
    type = 'pdf_transform', 
    gp   = pgpar(),
    tf   = NULL,
    transform = tf_scale(x, y)
  )
  
  pdf_add(doc, obj)
}



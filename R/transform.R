

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a transform specification for translation
#' 
#' @param x,y translation 
#' @return translation specification
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tf_translate <- function(x, y) {
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
#' Create a transform specification for rotation
#' 
#' @param rads rotation angle in radians
#' @param x,y location to rotate around
#' @return rotation specification
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tf_rotate <- function(rads, x = 0, y = 0) {
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
#' Create a transform specification for scaling
#' 
#' @param x,y scale amount in each direction. If 'y' value is not specified
#'        it is made the same as the 'x' value
#' @return scale specification
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tf_scale <- function(x, y = x) {
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
#' Modify global transformation matrix with new translation
#' 
#' Global transformations are cumulative, and these is no operation to reset
#' the global transformation.
#"
#' For local transformations use the \code{tf} argument for individual objects.
#' 
#' @inheritParams pdf_line
#' @inheritParams tf_translate
#' @return \code{pdf_doc}
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
#' Modify global transformation matrix with new rotation
#' 
#' Global transformations are cumulative, and these is no operation to reset
#' the global transformation.
#"
#' For local transformations use the \code{tf} argument for individual objects.
#' 
#' @inheritParams pdf_line
#' @inheritParams tf_rotate
#' @return \code{pdf_doc}
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
#' Modify global transformation matrix with new scaling
#' 
#' Global transformations are cumulative, and these is no operation to reset
#' the global transformation.
#"
#' For local transformations use the \code{tf} argument for individual objects.
#' 
#' @inheritParams pdf_line
#' @inheritParams tf_scale
#' @return \code{pdf_doc}
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






if (FALSE) {
  
  tl <- structure(
    list(
      tf_scale(12),
      tf_rotate(100),
      tf_translate(12, 12)
    ),
    class = "pdf_transform_list"
  )
  
  as.character(tl)
  
  
}






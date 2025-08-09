

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' build a transform object for translation
#' 
#' @param x,y translation 
#' @return translation object
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_translate <- function(x, y) {
  structure(
    list(x = x, y = y),
    class = c('pdf_transform', 'pdf_translate')
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert transform to character string
#' 
#' @param x object
#' @param ... ignored
#' @return string
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.pdf_translate <- function(x, ...) {
  glue::glue_data(x, "1 0 0 1 {x} {y} cm")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' build a transform object for rotation
#' 
#' @param rads rotation angle in radians
#' @param x,y location to rotate around
#' @return rotation object
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_rotate <- function(rads, x = 0, y = 0) {
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
#' build a transform object for rotation
#' 
#' @param x,y scale amount. 
#' @return scale object
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_scale <- function(x, y = x) {
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


if (FALSE) {
  
  tl <- structure(
    list(
      pdf_scale(12),
      pdf_rotate(100),
      pdf_translate(12, 12)
    ),
    class = "pdf_transform_list"
  )
  
  as.character(tl)
  
  
}








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Define a clipping rectangle
#'
#' @param x,y position
#' @param width,height size
#'
#' @return clipping rectangle specification
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clip_rect <- function(x, y, width, height) {
  structure(
    list(x = x, y = y, width = width, height = height),
    class = c('clip', 'clip_rect')
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert clipping spec into PDF string
#' @param x clip object
#' @param ... ignored
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.clip_rect <- function(x, ...) {
  glue::glue_data(x, "{x} {y} {width} {height} re W n")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Define a clipping polygon
#'
#' @param xs,ys vertices
#' @return clipping polygon specification
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clip_polygon <- function(xs, ys) {
  structure(
    list(xs = xs, ys = ys),
    class = c('clip', 'clip_polygon')
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname as.character.clip_rect
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.clip_polygon <- function(x, ...) {
  lines <- paste(x$xs[-1], x$ys[-1], 'l', collapse = ' ')
  glue::glue_data(x, "{xs[1]} {ys[1]} m {lines} W n") 
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add a rectangular clip path
#' @inheritParams pdf_line
#' @inheritParams clip_rect
#' @return pdf_doc
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_clip_rect <- function(doc, x, y, width, height, ..., gp = pgpar(), tf = NULL) {
  gp <- modifyList(gp, list(...))
  
  obj <- pdf_stream(
    type = 'clip', 
    gp   = gp,
    tf   = tf,
    clip_path = clip_rect(x = x, y = y, width = width, height = height)
  )
  
  pdf_add(doc, obj)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a polygonal clip
#' @param xs,ys vertices
#' @inheritParams pdf_line
#' @return pdf_doc
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_clip_polygon <- function(doc, xs, ys, ..., gp = pgpar(), tf = NULL) {
  gp <- modifyList(gp, list(...))
  
  obj <- pdf_stream(
    type = 'clip', 
    gp   = gp,
    tf   = tf,
    clip_path = clip_polygon(xs = xs, ys = ys)
  )
  
  pdf_add(doc, obj)
}






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname as.character.clip_rect
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.clip_list <- function(x, ...) {
  if (length(x) == 0) {
    character(0)
  } else {
    res <- vapply(x, as.character, character(1))
    paste(res, collapse = "\n")
  }
}



if (FALSE) {
  
  clips <- structure(
    list(
      clip_rect(0, 0, 100, 100),
      clip_rect(20, 20, 80, 80)
    ),
    class = c("clip", "clip_list")
  )
  
  as.character(clips[[1]])
  as.character(clips) |> cat()

}



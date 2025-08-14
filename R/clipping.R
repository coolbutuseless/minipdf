

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper to sanity check arguments
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_numeric_1 <- function(x) {
  is.numeric(x) &&
    !is.na(x) && 
    length(x) == 1
}

is_numeric_n <- function(x) {
  is.numeric(x) && !anyNA(x) && length(x) > 0
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Define a clipping rectangle for use as a \code{clip} argument
#'
#' @param x,y position
#' @param width,height size
#'
#' @return clipping rectangle specification
#' @examples
#' doc <- create_pdf() |>
#'    pdf_rect(0, 0, 100, 100, clip = clip_rect(50, 50, 200, 200))
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clip_rect <- function(x, y, width, height) {
  
  stopifnot(exprs = {
    is_numeric_1(x)
    is_numeric_1(y)
    is_numeric_1(width)
    is_numeric_1(height)
  })
  
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
#' Define a clipping polygon for use as a \code{clip} argument
#'
#' @inheritParams pdf_polygon
#' @param rule fill rule. 'winding' or 'evenodd'.  Default: 'winding'
#' @return clipping polygon specification
#' @examples
#' doc <- create_pdf() |>
#'    pdf_rect(0, 0, 100, 100, clip = clip_polygon(xs = c(0, 100, 100), 
#'    ys = c(0, 0, 100)))
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clip_polygon <- function(xs, ys, id = NULL, rule = 'winding') {
  
  stopifnot(exprs = {
    is_numeric_n(xs)
    is_numeric_n(ys)
    length(xs) == length(ys)
    rule %in% c('winding', 'evenodd')
  })
  
  if (!is.null(id)) {
    stopifnot(exprs = {
      is_numeric_n(id)
      length(id) == length(xs)
    })
  }
  
  structure(
    list(xs = xs, ys = ys, id = id, rule = rule),
    class = c('clip', 'clip_polygon')
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname as.character.clip_rect
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.clip_polygon <- function(x, ...) {
  # lines <- paste(x$xs[-1], x$ys[-1], 'l', collapse = ' ')
  # glue::glue_data(x, "{xs[1]} {ys[1]} m {lines} W n") 
  # print(x)
  clip_rule <- ifelse(identical(x$rule, 'winding'), 'W', 'W*')
  # print(clip_rule)
  
  if (is.null(x$id)) {
    lines <- paste(x$xs[-1], x$ys[-1], 'l', collapse = ' ')
    s <- glue::glue_data(x, "{xs[1]} {ys[1]} m {lines} h {clip_rule} n") 
  } else {
    id <- factor(x$id, levels = unique(x$id))
    xs_all <- split(x$xs, id)
    ys_all <- split(x$ys, id)
    
    
    polys <- lapply(seq_along(xs_all), function(i) {
      xs <- xs_all[[i]]
      ys <- ys_all[[i]]
      
      lines <- paste(xs[-1], ys[-1], 'l', collapse = ' ')
      
      glue::glue(
        "{xs[1]} {ys[1]} m 
         {lines} h"
      ) 
    })
    
    s <- paste(polys, collapse = "\n")
    
    # Final paoint
    s <- paste(s, paste(clip_rule, "n"), sep = "\n")
  }
  
  s
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add a global clipping rectangle to a PDF doc
#' 
#' Clipping regions are cumulative, and these is no operation to expand the 
#' global clipping region.
#"
#" The global clipping regeion is reset when a new page is created.  Otherwise
#' use local clipping with the \code{clip} argument to individual objects.
#'
#' @inheritParams pdf_line
#' @inheritParams clip_rect
#' @return \code{pdf_doc}
#' @examples
#' doc <- create_pdf() |>
#'    pdf_clip_rect(0, 0, 200, 200)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_clip_rect <- function(doc, x, y, width, height, tf = NULL) {
  
  obj <- pdf_stream(
    type = 'clip', 
    gp   = NULL,
    tf   = tf,
    clip_path = clip_rect(x = x, y = y, width = width, height = height)
  )
  
  pdf_add(doc, obj)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add a global clipping polygon to a PDF doc
#' 
#' Clipping regions are cumulative, and these is no operation to expand the 
#' global clipping region.
#"
#" The global clipping regeion is reset when a new page is created.  Otherwise
#' use local clipping with the \code{clip} argument to individual objects.
#" 
#' @inheritParams clip_polygon
#' @inheritParams pdf_line
#' @return \code{pdf_doc}
#' @examples
#' doc <- create_pdf() |>
#'    pdf_clip_polygon(xs = c(0, 100, 100), ys = c(0, 0, 100))
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_clip_polygon <- function(doc, xs, ys, id = NULL, rule = 'winding', tf = NULL) {
  
  obj <- pdf_stream(
    type = 'clip', 
    gp   = NULL,
    tf   = tf,
    clip_path = clip_polygon(xs = xs, ys = ys, id = id, rule = rule)
  )
  
  pdf_add(doc, obj)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname as.character.clip_rect
#' @exportS3Method
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.clip_list <- function(x, ...) {
  if (length(x) == 0) {
    character(0)
  } else {
    res <- vapply(x, as.character, character(1))
    paste(res, collapse = "\n")
  }
}



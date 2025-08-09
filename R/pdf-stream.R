

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

  switch(
    type,
    line = {
      s <- glue::glue_data(x, "{x1} {y1} m {x2} {y2} l S")
    },
    rect = {
      s <- glue::glue_data(x, "{x} {y} {width} {height} re {paint}")
    },
    polyline = {
      lines <- paste(x$xs[-1], x$ys[-1], 'l', collapse = ' ')
      s <- glue::glue_data(x, "{xs[1]} {ys[1]} m {lines} S") # 'S' = stroke (without closing)
    },
    polygon = {
      lines <- paste(x$xs[-1], x$ys[-1], 'l', collapse = ' ')
      s <- glue::glue_data(x, "{xs[1]} {ys[1]} m {lines} {paint}") 
    },
    circle = {
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Bezier offset. See:
      # stackoverflow.com/questions/1734745/how-to-create-circle-with-b%C3%A9zier-curves
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      x$b     <- 0.552284749831 * x$r
      
      s <- glue::glue_data(
        x, 
        "{x+r} {y} m
         {x+r} {y+b}  {x+b} {y+r}  {x}   {y+r} c
         {x-b} {y+r}  {x-r} {y+b}  {x-r} {y}   c
         {x-r} {y-b}  {x-b} {y-r}  {x}   {y-r} c
         {x+b} {y-r}  {x+r} {y-b}  {x+r} {y}   c
         {paint}"
      ) 
    },
    text = {
      
      # Mode
      # 0 Fill text. Normal. Default
      # 1 Stroke text
      # 2 Fill then stroke
      # 3 NO fill or stroke. Invisible
      # 4 Fill text and add to path for clipping
      # 5 Stroke text and add to path for clipping
      # 6 Fill, then stroke text and add to path for clipping
      # 7 Add text to path for clipping
      font_ref <- gp_to_font_ref(x$gp)
      s <- glue::glue_data(
        x,
        "BT
        /{font_ref} {fontsize} Tf
        {x} {y} Td
        {mode} Tr
        ({text}) Tj
        ET"
      )
    },
    
    image = {
      
      x$w <- ncol(x$im)
      x$h <- nrow(x$im)
      
      s <- glue::glue_data(
        x, 
        "{w * scale} 0 0 {h * scale} {x} {y} cm",
        "/Im{idx_offset} Do",
        .sep = "\n"
      )
    },
    
    stop("Unknown stream: ", deparse1(class(x)))
  )

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add graphics state operators
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gs <- gp_to_gs_operators(x$gp)
  s  <- paste(gs, s, sep = "\n")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add reference to graphics state dict
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(x$gs_ref)) {
    gs_ref <- glue::glue("/GS{x$gs_ref} gs")
    s <- paste(gs_ref, s, sep = "\n")
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
#' @param doc pdf_doc
#' @param x1,y1,x2,y2 endpoints
#' @param gp A named list \code{gp} object created by \code{\link{pgpar}()}
#' @param ... further arguments to be added to \code{gp}
#' @return pdf_doc
#' @export
#' @importFrom utils modifyList
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_line <- function(doc, x1, y1, x2, y2, ..., gp = pgpar()) {
  gp <- modifyList(gp, list(...))
  
  obj <- pdf_stream(
    type = 'line', 
    gp   = gp,
    x1 = x1, y1 = y1, x2 = x2, y2 = y2, gp = gp
  )
  
  pdf_add(doc, obj)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a rect
#' @param x,y position
#' @param width,height size
#' @inheritParams pdf_line
#' @return pdf_doc
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_rect <- function(doc, x, y, width, height, ..., gp = pgpar()) {
  gp <- modifyList(gp, list(...))
  
  obj <- pdf_stream(
    type = 'rect', 
    gp   = gp,
    x = x, y = y, width = width, height = height
  )
  
  pdf_add(doc, obj)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a polyline
#' @param xs,ys vertices
#' @inheritParams pdf_line
#' @return pdf_doc
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_polyline <- function(doc, xs, ys, ..., gp = pgpar()) {
  gp <- modifyList(gp, list(...))
  
  obj <- pdf_stream(
    type = 'polyline', 
    gp   = gp,
    xs = xs, ys = ys
  )
  
  pdf_add(doc, obj)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a polygon
#' @param xs,ys vertices
#' @inheritParams pdf_line
#' @return pdf_doc
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_polygon <- function(doc, xs, ys, ..., gp = pgpar()) {
  gp <- modifyList(gp, list(...))
  
  obj <- pdf_stream(
    type = 'polygon', 
    gp   = gp,
    xs = xs, ys = ys
  )
  
  pdf_add(doc, obj)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a polygon
#' @param x,y,r position and radius
#' @inheritParams pdf_line
#' @return pdf_doc
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_circle <- function(doc, x, y, r, ..., gp = pgpar()) {
  gp <- modifyList(gp, list(...))
  
  obj <- pdf_stream(
    type = 'circle', 
    gp   = gp,
    x = x, y = y, r = r
  )
  
  pdf_add(doc, obj)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create text
#' @param x,y position
#' @param text text
#' @param fontsize Default: 12
#' @param mode Default: 0
#' \itemize{
#'   \item{0 - Fill text. Normal. Default}
#'   \item{1 - Stroke text}
#'   \item{2 - Fill then stroke}
#'   \item{3 - NO fill or stroke. Invisible}
#'   \item{4 - Fill text and add to path for clipping}
#'   \item{5 - Stroke text and add to path for clipping}
#'   \item{6 - Fill, then stroke text and add to path for clipping}
#'   \item{7 - Add text to path for clipping}
#' }
#' @inheritParams pdf_line
#' @return pdf_doc
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_text <- function(doc, text, x, y, fontsize = 12, mode = 0, ..., gp = pgpar()) {
  gp <- modifyList(gp, list(...))
  
  obj <- pdf_stream(
    type = 'text', 
    gp   = gp,
    x = x, y = y, text = text, mode = mode, fontsize = fontsize
  )
  
  pdf_add(doc, obj)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add image
#' 
#' @inheritParams pdf_line
#' @param im integer matrix [0, 255]
#' @param x,y position
#' @param scale scale for image
#' @return pdf_doc
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_image <- function(doc, im, x, y, scale = scale, ..., gp = pgpar()) {
  stopifnot(is.matrix(im))
  stopifnot(is.integer(im))
  gp <- modifyList(gp, list(...))
  
  # idx_offset <- length(doc$image) + 1L
  # doc$image[[idx_offset + 1L]] <- im
  idx_offset <- 1
  
  obj <- pdf_stream(
    type = 'image', 
    gp   = gp,
    im = im,
    idx_offset = idx_offset,
    x = x, y = y, scale = scale
  )
  
  pdf_add(doc, obj)
}






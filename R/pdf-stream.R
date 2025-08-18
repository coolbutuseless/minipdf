

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a \code{pdf_stream} object
#' 
#' @param ... named arguments
#' @return \code{pdf_stream} object
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
#' Check if an object is a \code{pdf_stream}
#' @param x object to test
#' @return logical
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_stream <- function(x) {
  isTRUE(inherits(x, 'pdf_stream'))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert \code{pdf_stream} to character
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

  # Should this stream save/restore state for its operation?
  # Most things = Yes!
  # For clipping paths, No - as we want the clipping
  # path to persist for future operations
  restore_state <- TRUE
  
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
      if (is.null(x$id)) {
        lines <- paste(x$xs[-1], x$ys[-1], 'l', collapse = ' ')
        s <- glue::glue_data(x, "{xs[1]} {ys[1]} m {lines} {paint}") 
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
        s <- paste(s, paint, sep = "\n")
      }
    },
    clip = {
      restore_state <- FALSE
      s <- as.character(x$clip_path)
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
      font_ref <- font_to_font_ref(x$fontfamily, x$fontface)
      s <- glue::glue_data(
        x,
        "BT
        /{font_ref} {fontsize} Tf
        {x} {y} Td
        {mode} Tr
        {as_pdf_text(text)} Tj
        ET"
      )
    },
    
    pdf_transform = {
      restore_state <- FALSE
      s <- as.character(x$transform)
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
  # transforms
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(x$tf)) {
    tf <- x$tf
    stopifnot(is.list(tf))
    if (!inherits(tf, "pdf_transform")) {
      class(tf) <- 'pdf_transform_list'
    }
    tf <- as.character(tf)
    s <- paste(tf, s, sep = "\n")
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # clipping
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(x$clip)) {
    clip <- x$clip
    stopifnot(is.list(clip))
    if (!inherits(clip, "clip")) {
      class(clip) <- c("clip", "clip_list")
    }
    clip <- as.character(clip)
    s <- paste(clip, s, sep = "\n")
  }
  
  if (type != 'clip') {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add graphics state operators. Not needed for clipping
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    gs <- gp_to_gs_operators(x$gp)
    s  <- paste(gs, s, sep = "\n")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add reference to graphics state dict. Not needed for clipping.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(x$gs_ref)) {
      gs_ref <- glue::glue("/GS{x$gs_ref} gs")
      s <- paste(gs_ref, s, sep = "\n")
    }
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Push/pop the local graphics state
  #   - always do this for graphics operations
  #   - never do this for clipping definitions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(restore_state)) {
    s <- paste('q', s, 'Q', sep = "\n")
  }
  
  
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
#' Print \code{pdf_stream}
#' 
#' @param x pdf_stream
#' @param ... ignored
#' @return None
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.pdf_stream <- function(x, ...) {
  cat("<stream: ")
  type <- attr(x, 'type', exact = TRUE)
  cat(type, ">\n", sep = "")
  cat(as.character(x, ...), "\n", sep = "")
  invisible(x)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add a line to a PDF doc
#' 
#' @inheritParams pdf_newpage
#' @param x1,y1,x2,y2 endpoints
#' @param gp A named list \code{gp} object created by \code{\link{pgpar}()}
#' @param tf either a single transform (\code{tf_translate()}, \code{tf_scale()},
#'        \code{tf_rotate()}), or a list of these transforms.  Default: NULL,
#'        no local transformation applied (global transformations still apply)
#' @param clip either a single clip (\code{clip_rect()}, \code{clip_polygon()}),
#'        or a list of these clips.  Default: NULL,
#'        no local clipping applied (global clipping still applicable)
#' @param ... further arguments to be added to \code{gp}
#' @return \code{pdf_doc}
#' @examples
#' doc <- create_pdf() |>
#'    pdf_line(10, 10, 100, 100, col = 'red')
#' @export
#' @importFrom utils modifyList
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_line <- function(doc, x1, y1, x2, y2, ..., gp = pgpar(), 
                     tf = NULL, clip = NULL) {
  
  stopifnot(exprs = {
    is_numeric_1(x1)
    is_numeric_1(y1)
    is_numeric_1(x2)
    is_numeric_1(y2)
  })
  
  gp <- modifyList(gp, list(...))
  
  obj <- pdf_stream(
    type = 'line', 
    gp   = gp,
    tf   = tf,
    clip = clip,
    x1 = x1, y1 = y1, x2 = x2, y2 = y2, gp = gp
  )
  
  pdf_add(doc, obj)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add a rectangle to a PDF doc
#' 
#' @param x,y position of lower left of rectangle
#' @param width,height width of height of rectangle
#' @inheritParams pdf_line
#' @return \code{pdf_doc}
#' @examples
#' doc <- create_pdf() |>
#'    pdf_rect(10, 10, 100, 100, gp = pgpar(fill = 'red'))
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_rect <- function(doc, x, y, width, height, ..., gp = pgpar(), 
                     tf = NULL, clip = NULL) {
  
  stopifnot(exprs = {
    is_numeric_1(x)
    is_numeric_1(y)
    is_numeric_1(width)
    is_numeric_1(height)
  })
  
  gp <- modifyList(gp, list(...))
  
  obj <- pdf_stream(
    type = 'rect', 
    gp   = gp,
    tf   = tf,
    clip = clip,
    x = x, y = y, width = width, height = height
  )
  
  pdf_add(doc, obj)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add a polyline to a PDF doc
#' 
#' @inheritParams pdf_polygon
#' @return \code{pdf_doc}
#' @examples
#' doc <- create_pdf() |>
#'    pdf_polyline(xs = c(100, 200, 200), ys = c(100, 100, 200))
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_polyline <- function(doc, xs, ys, ..., gp = pgpar(), 
                         tf = NULL, clip = NULL) {
  
  stopifnot(exprs = {
    is_numeric_n(xs)
    is_numeric_n(ys)
  })
  
  gp <- modifyList(gp, list(...))
  
  obj <- pdf_stream(
    type = 'polyline', 
    gp   = gp,
    tf   = tf,
    clip = clip,
    xs = xs, ys = ys
  )
  
  pdf_add(doc, obj)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add a polygon to a PDF doc
#' 
#' @param xs,ys vertices
#' @param id A numeric vector used to searpate vertices into multiple polygons.
#'        All vertices with the same id belong to the same polygon. Default: NULL
#'        means that all vertices belong to a single polygon.
#' @inheritParams pdf_line
#' @return \code{pdf_doc}
#' @examples
#' doc <- create_pdf() |>
#'    pdf_polygon(xs = c(100, 200, 200), ys = c(100, 100, 200))
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_polygon <- function(doc, xs, ys, id = NULL, ..., gp = pgpar(), 
                        tf = NULL, clip = NULL) {
  
  stopifnot(exprs = {
    is_numeric_n(xs)
    is_numeric_n(ys)
    is.null(id) || (is_numeric_n(id) && length(id) == length(xs))
  })

  gp <- modifyList(gp, list(...))
  
  obj <- pdf_stream(
    type = 'polygon', 
    gp   = gp,
    tf   = tf,
    clip = clip,
    xs   = xs, 
    ys   = ys,
    id   = id
  )
  
  pdf_add(doc, obj)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add a circle to a PDF doc
#' 
#' @param x,y,r position of centre and radius of circle
#' @inheritParams pdf_line
#' @return \code{pdf_doc}
#' @examples
#' doc <- create_pdf() |>
#'    pdf_circle(x = 200, y = 200, r = 50)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_circle <- function(doc, x, y, r, ..., gp = pgpar(), 
                       tf = NULL, clip = NULL) {
  
  stopifnot(exprs = {
    is_numeric_1(x)
    is_numeric_1(y)
    is_numeric_1(r)
  })
  
  gp <- modifyList(gp, list(...))
  
  obj <- pdf_stream(
    type = 'circle', 
    gp   = gp,
    tf   = tf,
    clip = clip,
    x = x, y = y, r = r
  )
  
  pdf_add(doc, obj)
}

# ff <- Hmisc::Cs(
# Helvetica            ,
# Helvetica-Bold       ,
# Helvetica-Oblique    ,
# Helvetica-BoldOblique,
# Courier              ,
# Courier-Bold         ,
# Courier-Oblique      ,
# Courier-BoldOblique,
# Times-Roman          ,
# Times-Bold           ,
# Times-Italic  ,
# Times-BoldItalic     ,
# Symbol               ,
# ZapfDingbats         
# )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add text to PDF
#' 
#' @param x,y position
#' @param text character string to render
#' @param fontfamily Font name. Default: 'Helvetica'. One of: "Helvetica", 
#'        "Courier", "Times", "Symbol", "ZapfDingbats"
#' @param fontface Font styling. Default: 'plain'. One of: 'plain', 'bold', 
#'        'italic', 'bold.italic'
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
#' @return \code{pdf_doc}
#' @examples
#' doc <- create_pdf() |>
#'    pdf_text("Hello", x = 20, y = 20, fontsize = 50)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_text <- function(doc, text, x, y, fontfamily = 'Helvetica', fontface = 'plain', 
                     fontsize = 12, mode = 0, ..., gp = pgpar(),
                     tf = NULL, clip = NULL) {
  
  stopifnot(exprs = {
    is.character(text)
    is_numeric_1(x)
    is_numeric_1(y)
    is_numeric_1(fontsize)
    is_numeric_1(mode)
  })
  
  gp <- modifyList(gp, list(...))
  
  obj <- pdf_stream(
    type = 'text', 
    gp   = gp,
    tf   = tf,
    clip = clip,    
    x = x, y = y, text = text, mode = mode, 
    fontfamily = fontfamily, fontface = fontface,
    fontsize = fontsize
  )
  
  pdf_add(doc, obj)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add image to PDF
#' 
#' @inheritParams pdf_line
#' @param im Image represented as a numeric matrix or array with all values 
#'        in range [0, 255]
#' @param x,y position of bottom-left corner of image
#' @param scale scale factor when rendering image Default: 1
#' @param interpolate Should pixel values be interpolated? Default: FALSE
#' @return \code{pdf_doc}
#' @examples
#' im <- matrix(1:100, 10, 10)
#' doc <- create_pdf() |>
#'    pdf_image(im, 20, 20, scale = 2)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_image <- function(doc, im, x, y, scale = 1, interpolate = FALSE, ..., gp = pgpar(), 
                      tf = NULL, clip = NULL) {
  
  # Sanity check
  stopifnot(exprs = {
    is.array(im) || is.matrix(im)
    is_bytes(im)
    is_numeric_1(x)
    is_numeric_1(y)
    is_numeric_1(scale)
    is.logical(interpolate)
  })
  

  # Assemble graphical parameters
  gp <- modifyList(gp, list(...))
  
  # Insert the image data into the document at the top level
  idx_offset <- length(doc$image) + 1L
  attr(im, 'interpolate') <- isTRUE(interpolate)
  doc$image[[idx_offset]] <- im
  
  # This stream inserts a "Do" reference to the iamge
  obj <- pdf_stream(
    type = 'image', 
    gp   = gp,
    tf   = tf,
    clip = clip,
    im   = im,
    interpolate = isTRUE(interpolate),
    idx_offset = idx_offset,
    x = x, y = y, scale = scale
  )
  
  pdf_add(doc, obj)
}



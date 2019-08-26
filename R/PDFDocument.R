
pdf_fonts <- c('Times-Roman', 'Times-Bold', 'Times-Italic', 'Times-BoldItalic',
               'Helvetica', 'Helvetica-Bold', 'Helvetica-Oblique', 'Helvetica-BoldOblique',
               'Courier'  , 'Courier-Bold'  , 'Courier-Oblique'  , 'Courier-BoldOblique'  ,
               'Symbol', 'ZapfDingbats')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create rotation transform about a given coord
#'
#' @param angle rotation angle in degrees
#' @param x,y centre of rotation
#'
#' @return transofmration string
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_rotate_transform_about_coord <- function(x, y, angle) {

  translate_origin1 <- glue("1 0 0 1 { x} { y} cm")
  translate_origin2 <- glue("1 0 0 1 {-x} {-y} cm")

  cosQ   <- round(cos(angle * pi/180), 4)
  sinQ   <- round(sin(angle * pi/180), 4)
  trans  <- c(cosQ, sinQ, -sinQ, cosQ, 0, 0, 'cm')
  rotate <- paste(trans, collapse = " ")

  if (x == 0 && y == 0) {
    rotate
  } else {
    paste(translate_origin1, rotate, translate_origin2)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get the path painting style based upon which of fill/stroke are NULL
#'
#' @param fill,stroke colours
#'
#' @return one of 'n', 's', 'f', 'b'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_path_paint_style <- function(fill, stroke) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For path painting operations. See Table60 PDF3200-1:2008 (page 135)
  # S  - stroke
  # s  - close and stroke (equivalent to 'h S')
  # f  - fil the path (using non-zero winding number)
  # f* - fill the path using even odd rule
  # B  - fill and then stroke the path (nonzero winding)
  # B* - fill and then stroke the path (even-odd)
  # b  - close, fill & stroke (nonzero winding)
  # b* - close, fill & stroek (even odd)
  # n  - do nothing. no-op
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(fill) && is.null(stroke)) {
    'n'
  } else if (is.null(fill) && !is.null(stroke)) {
    's'
  } else if (!is.null(fill) && is.null(stroke)) {
    'f'
  } else {
    'b'
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sanitise colour to 4-element RGBA
#'
#' When colour supplied as a vector, elements must in range [0, 255]
#'
#' @param colour specified as hex-colour (e.g. '#123456'),
#'        standard colour name (e.g. 'red'), 3-element RGB vector (alpha will
#'        be assumed to be 255), or 4-element RGB vector
#'
#' @return 4 element vector representing RGBA with each element in range [0, 1]
#'
#' @importFrom grDevices col2rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sanitise_colour_to_rgba_vec <- function(colour) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # NULL is used to indicate that fill/stroke is not happening
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(colour) || length(colour) == 0) {
    return(NULL)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If colour is a character assume it's an R standard colour, otherwise
  # process as if it is a 3 or 4-element numeric vector
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.character(colour)) {
    stopifnot(length(colour) == 1L)
    rgba_vec <- as.vector(grDevices::col2rgb(colour, alpha = TRUE))
  } else {
    stopifnot(is.numeric(colour))
    rgba_vec <- colour
    stopifnot(all(rgba_vec >= 0))
    stopifnot(all(rgba_vec <= 255))
    if (length(rgba_vec) == 3) {
      rgba_vec <- c(rgba_vec, 255)  # Add alpha
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Scale to range [0, 1] and sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rgba_vec <- rgba_vec / 255
  stopifnot(length(rgba_vec) == 4)


  rgba_vec
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Process 'fill' and 'stroke' colours into snippets of the stream specification
#'
#' @param fill,stroke 4 element RGBA vectors
#'
#' @return named list with pre-rendered text streams for fill, stroke and
#'         the painting tpe to be used i.e.. 'f' = just fill, 'b' = fill and stroke,
#'         'S' =  just stroke?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
process_fill_and_stroke <- function(fill, stroke) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert colours to standard RGBA[0,1] format
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fill   <- sanitise_colour_to_rgba_vec(fill)
  stroke <- sanitise_colour_to_rgba_vec(stroke)

  fill_spec   <- ifelse(is.null(fill)  , "", glue("{  fill[1]}   {fill[2]}   {fill[3]} rg"))
  stroke_spec <- ifelse(is.null(stroke), "", glue("{stroke[1]} {stroke[2]} {stroke[3]} RG"))

  list(
    fill_spec    = fill_spec,
    stroke_spec = stroke_spec,
    fill        = fill,
    stroke      = stroke
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create a graphics state dictionary string
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_gs_dict <- function(name, dict) {

  dict_string <- paste(
    paste0("/", names(dict)),
    unname(dict),
    collapse = " "
  )

  glue("/{name} <<{dict_string}>>")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Document Class
#'
#' R6 class for creating simple, single-page PDF documents
#'
#'
#' \itemize{
#' \item{\code{$initialize()}} \itemize{
#'   \item{\code{width,height}} - Page dimensions (pixels)
#'   \item{\code{font}} - name of font. One of: Times-Roman, Times-Bold, Times-Italic,
#'        Times-BoldItalic, Helvetica, Helvetica-Bold, Helvetica-Oblique,
#'        Helvetica-BoldOblique, Courier, Courier-Bold, Courier-Oblique,
#'        Courier-BoldOblique, Symbol, ZapfDingbats
#'   \item{\code{version}} - PDF version. Default: 1.2
#' }
#' \item{\code{$write_pdf()}} \itemize{
#'   \item{\code{filename}} - name of output file
#' }
#' \item{\code{$set_clip_rect()}} \itemize{
#'   \item{\code{x,y,width,height}} - dimensions of clipping rectangle
#' }
#' \item{\code{$add_text()}} \itemize{
#'   \item{\code{text}} - string
#'   \item{\code{x,y}} - location of text
#'   \item{\code{size}} - font size. Default: 12
#'   \item{\code{angle}} - rotation angle. Default: 0
#'   \item{\code{text_mode}} - Integer. 0 = fill only. 1 = stroke only. 2 = stroke and fill. Default: 0
#'   \item{\code{fill,stroke}} - Colours. May be given as hex-colour \code{'#123456'}, RGB vector \code{c(0, 200, 12)}, or RGBA vector e.g. \code{c(0, 156, 255, 255)}
#'   \item{\code{linewidth}} - line width in pixels
#'   \item{\code{linetype}} - Integer from 0 (solid) to 6 (twodash)
#' }
#' \item{\code{$add_rect()}} \itemize{
#'   \item{\code{x,y,width,height}} - location and size of rectangle
#'   \item{\code{fill,stroke,linewidth,linetype}} - See documentation for other methods
#' }
#' \item{\code{$add_circle()}} \itemize{
#'   \item{\code{x,y,r}} - Location and radius of circle
#'   \item{\code{fill,stroke,linewidth,linetype}} - See documentation for other methods
#' }
#' \item{\code{$add_line()}} \itemize{
#'   \item{\code{x1,y1,x2,y2}} - Coordinates of line endpoints
#'   \item{\code{stroke,linewidth,linetype}} - See documentation for other methods
#' }
#' \item{\code{$add_polyline()}} \itemize{
#'   \item{\code{xs,ys}} - vectors of coordinates of each point along the polyline
#'   \item{\code{stroke,linewidth,linetype}} - See documentation for other methods
#' }
#' \item{\code{$add_polygon()}} \itemize{
#'   \item{\code{xs,ys}} - vectors of coordinates of each point along the polygon
#'   \item{\code{fill,stroke,linewidth,linetype}} - See documentation for other methods
#' }
#' }
#'
#'
#' @import R6
#' @import glue
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFDocument <- R6::R6Class(
  "PDFDocument",
  public = list(
    header       = NULL,
    obj          = NULL,
    xref         = NULL,
    trailer      = NULL,
    width        = NULL,
    height       = NULL,
    n_start_objs = 0,
    ext_gs_state = NULL,

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # @name create_pdfdoc
    # @title Create a new PDF Document
    # @section R6 Usage: $new()
    # @description Create a new PDF Document
    #
    # @details
    # Object 1: /Catalog
    #       - points to Pages object
    # Object 2: /Pages
    #       - points to the single /Page object
    # Object 3: /Page
    #       - points to resources dictionary
    #       - sets media size
    #       - lists /Contents
    # Object 4: Resources dictionary
    # Object 5: /Font
    #       - Setting up a reference to a font
    # Object 6: The font definition
    #
    # @param width,height Page dimensions in pixels
    # @param font name of font. One of: Times-Roman, Times-Bold, Times-Italic,
    # Times-BoldItalic, Helvetica, Helvetica-Bold, Helvetica-Oblique, \
    # Helvetica-BoldOblique, Courier, Courier-Bold, Courier-Oblique,
    # Courier-BoldOblique, Symbol, ZapfDingbats
    # @param version PDF standard version number. default 1.2
    #
    # @return Blank PDF document
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(width = 400, height = 400, font = 'Helvetica', version = 1.2) {
      self$header <- glue::glue("%PDF-{version}")
      self$width  <- width
      self$height <- height

      if (!font %in% pdf_fonts) {
        stop("PDFDocument$initialize(): Unknown font: ", deparse(font), call. = FALSE)
      }

      self$obj <- c(
        "1 0 obj <</Type /Catalog /Pages 2 0 R>>\nendobj",
        "2 0 obj <</Type /Pages /Kids [3 0 R] /Count 1>>\nendobj",
        "3 0 obj<</Type /Page /Parent 2 0 R /Resources 4 0 R /MediaBox [0 0 {self$width} {self$height}] /Contents 6 0 R>>\nendobj",
        "4 0 obj<</Font <</F1 5 0 R>>/ExtGState<</GS0.50 <</ca 0.2>>>>>>\nendobj",
        glue("5 0 obj<</Type /Font /Subtype /Type1 /BaseFont /{font}>>\nendobj")
      )

      self$n_start_objs <- length(self$obj)


      self$reset_clip_rect()

      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Write the list of objects as the "/Contents" of the page
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_page_contents = function() {
      object_numbers <- setdiff(seq(length(self$obj)), seq(self$n_start_objs))
      if (is.null(object_numbers) || length(object_numbers) == 0) {
        warning("PDFDocument$update_page_contents(): No objects added", call. = FALSE)
        return(self)
      }
      contents_array <- glue("{object_numbers} 0 R")
      contents_array <- paste(contents_array, collapse = " ")
      self$obj[3]    <- glue("3 0 obj<</Type /Page /Parent 2 0 R /Resources 4 0 R /MediaBox [0 0 {self$width} {self$height}] /Contents [{contents_array}]>>\nendobj")
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update any external graphics state references
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_gs_references  = function() {
      ext_gs_state <- self$get_ext_gs_state()
      self$obj[4]  <- glue("4 0 obj<</Font <</F1 5 0 R>> {ext_gs_state} >>\nendobj")
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update a xref table based upon the current objects
    # and the offsets to the start of each object
    # Each xref entry must be exactly 20bytes (include CRLF).  Since I'm only
    # using CR, I need to add an extra space at the end of each xref entry
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_xref = function() {
      all_offsets    <- cumsum(nchar(c(self$header, self$obj)) + 1)
      object_offsets <- head(all_offsets, -1)
      self$xref <- c(
        "xref",
        glue("0 {length(self$obj) + 1L}"),
        "0000000000 65535 f ",
        sprintf("%010i 00000 n ", object_offsets)
      )
      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Calculate the offsets to the start of each of the objects.
    #  - this is needed for the cross-reference table
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    calc_offsets = function() {
      cumsum(nchar(c(self$header, self$obj)) + 1)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # What is he byte index of the start of the 'xref' table?
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    calc_startxref = function() {
      tail(self$calc_offsets(), 1)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update the trailer with the correct count of objects in the document
    # and the current location of 'startxref'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_trailer = function() {
      self$trailer <- c(
        glue::glue("trailer <</Size {length(self$obj) + 1}/Root 1 0 R>>"),
        "startxref",
        self$calc_startxref(),
        "%%EOF"
      )
      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add a graphics state dictionary
    #  /GS0.50 <</ca 0.5>>
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_gs_dict = function(name, dict) {
      gs_dict           <- create_gs_dict(name, dict)
      self$ext_gs_state <- c(self$ext_gs_state, gs_dict)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Get the current state of the ExtGSState dictionary as a string
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_ext_gs_state = function() {
      if (length(self$ext_gs_state) == 0) {
        return("")
      }

      gs <- unique(self$ext_gs_state)
      gs <- paste(gs, collapse = " ")
      glue("/ExtGState<<{gs}>>")
    },



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add alpha to the stroke and fill for the current external graphics state
    #   - set alpha to 1 if undefined
    #   - round to 2 decimal places and create a dictionary name by concatentating
    #     the string representation of the 2 alphas
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_alpha = function(fill_alpha, stroke_alpha) {

      if (is.null(fill_alpha  )) fill_alpha   <- 1
      if (is.null(stroke_alpha)) stroke_alpha <- 1

      fill_alpha   <- round(fill_alpha, 2)
      stroke_alpha <- round(stroke_alpha, 2)

      name <- paste0("GS", fill_alpha, stroke_alpha)
      dict <- list(ca = fill_alpha, CA = stroke_alpha)

      self$add_gs_dict(name, dict)

      name
    },



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add an object to the PDF document
    # - Calculates the new object number
    # - Calculates and inserts the Dictionary with just  'Length' specified
    # - wraps the object in "obj" - "endobj"
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_obj = function(obj) {
      new_obj <- c(
        glue("{length(self$obj) + 1} 0 obj"),
        glue("<</Length {nchar(obj)}>>"),
        obj,
        "endobj"
      )

      new_obj  <- paste(new_obj, collapse = "\n")
      self$obj <- c(self$obj, new_obj)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add a stream object to the document
    #  - calculates object number
    #  - calcualtes total strream length
    #  - wraps stream in stream/endstream and obj/endobj
    #  - adds it to the vector of objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_stream_obj = function(stream) {
      new_obj <- c(
        glue("{length(self$obj) + 1} 0 obj"),
        glue("<</Length {nchar(stream)}>>"),
        "stream",
        stream,
        "endstream",
        "endobj"
      )

      new_obj  <- paste(new_obj, collapse = "\n")
      self$obj <- c(self$obj, new_obj)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create a stream from the given geom.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_geom_stream = function(geom, fill, stroke, angle, rx, ry, linewidth, linetype = 4) {

      if (is.null(angle) || angle == 0) {
        rotate_spec <- ""
      } else {
        rotate_spec <- create_rotate_transform_about_coord(rx, ry, angle)
      }


      linetype_spec <- switch(
        linetype + 1,
        "",                # 0 = blank
        ""       ,         # 1 = solid
        "[3] 0 d",         # 2 = dashed
        "[1 3] 0 d",       # 3 = dotted
        "[1 1 3 1] 0 d",   # 4 = dotdash
        "[5] 0 d",         # 5 = longdash
        "[1 1 3 1] 0 d",   # 6 = twodash
        "",
      )

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Ready the colour specification and graphics state for alpha setting
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      colour  <- process_fill_and_stroke(fill, stroke)
      gs_name <- self$add_alpha(colour$fill[4], colour$stroke[4])

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create and add the stream
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      stream <- glue("q {rotate_spec} {linetype_spec} {linewidth} w /{gs_name} gs {colour$fill_spec} {colour$stroke_spec} {geom} Q")
      self$add_stream_obj(stream)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create a stream from the given clipping path
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_clip_stream = function(clip_path) {

      # 'W' to set this as a clipping path.
      # 'n' for "don't actually draw the path"
      stream <- glue("{clip_path} W n")
      self$add_stream_obj(stream)

      invisible(self)
    }

  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name create_pdfdoc
#' @title Create a new PDF Document
#' @section R6 Usage: $new()
#' @description Create a new PDF Document
#' @param width,height Page dimensions in pixels
#' @param version PDF standard version number. default 1.2
#' @param font name of font. One of: Times-Roman, Times-Bold, Times-Italic,
#'        Times-BoldItalic, Helvetica, Helvetica-Bold, Helvetica-Oblique, \
#'        Helvetica-BoldOblique, Courier, Courier-Bold, Courier-Oblique,
#'        Courier-BoldOblique, Symbol, ZapfDingbats
#'
#' @return Blank PDF document
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pdfdoc <- function(width = 400, height = 400, font = 'Helvetica', version = 1.2) {
  PDFDocument$new(width = width, height = height, version = version)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name set_clip_rect
#' @title Set the clipping rectangle
#' @section R6 Usage: $set_clip_rect()
#' @description Sets the global clipping rectangle for the document
#'
#' @param pdfdoc PDF document. An R6 object of class \code{PDFDocument}. Created with \code{create_pdfdoc()} or \code{PDFDocument$new()}
#' @param x,y,width,height dimenensions of rectangle
#'
#' @return PDF document with clipping rectangle set added
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NULL
PDFDocument$set(
  "public", "set_clip_rect",
  function(x, y, width, height) {
    stream <- glue("
{x        } {y         } m
{x + width} {y         } l
{x + width} {y + height} l
{x        } {y + height} l
{x        } {y         } l
")
    self$add_clip_stream(stream)
    invisible(self)
  }
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name reset_clip_rect
#' @title Reset the clipping rectangle to encompass the entire page
#' @section R6 Usage: $reset_clip_rect()
#' @description Sets the global clipping rectangle to encompass the document
#'
#' @param pdfdoc PDF document. An R6 object of class \code{PDFDocument}. Created with \code{create_pdfdoc()} or \code{PDFDocument$new()}
#'
#' @return PDF document with clipping rectangle set added
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NULL
PDFDocument$set(
  "public", "reset_clip_rect",
  function() {
    self$set_clip_rect(x = 0, y = 0, width = self$width, height = self$height)

    invisible(self)
  }
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name add_text
#' @title Add text
#' @section R6 Usage: $add_text()
#' @description Adds text to the current document
#'
#' @param pdfdoc PDF document. An R6 object of class \code{PDFDocument}. Created with \code{create_pdfdoc()} or \code{PDFDocument$new()}
#' @param text text to add (string)
#' @param x,y location of text
#' @param size font size. Default: 12
#' @param angle rotation angle measured in degrees anti-clockwise. Default: 0
#' @param fill,stroke colours. May be given as hex-colour \code{'#123456'}, RGB vector \code{c(0, 200, 12)}, or RGBA vector e.g. \code{c(0, 156, 255, 255)}
#' @param text_mode Integer. 0 = fill, 1 = stroke, 2 = fill and stroke. Default: 0
#' @param linewidth line width (pixels)
#' @param linetype Integer from 0 (solid) to 6 (twodash)
#'
#' @return PDF document with line added
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NULL
PDFDocument$set(
  "public", "add_text",
  function(text, x, y, size = 12, angle = 0, fill = '#000000', stroke = '#000000',
           text_mode = 0, linewidth = 1, linetype = 0) {

    geom <- glue("BT /F1 {size} Tf {x} {y} Td {text_mode} Tr ({text})Tj ET")
    self$add_geom_stream(geom = geom, fill = fill, stroke = stroke, angle = angle,
                         rx = x, ry = y,
                         linewidth = linewidth, linetype = linetype)

    invisible(self)
  }
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name add_rect
#' @title Add rectangle
#' @section R6 Usage: $add_rect()
#' @description Adds rectangle to the current document
#'
#' @param pdfdoc PDF document. An R6 object of class \code{PDFDocument}. Created with \code{create_pdfdoc()} or \code{PDFDocument$new()}
#' @param x,y,width,height location and size of rectangle
#' @param fill,stroke Colours. May be given as hex-colour \code{'#123456'}, RGB vector \code{c(0, 200, 12)}, or RGBA vector e.g. \code{c(0, 156, 255, 255)}
#' @param linewidth line width (pixels)
#' @param linetype Integer from 0 (solid)u to 6 (twodash)
#'
#' @return PDF document with rectangle added
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NULL
PDFDocument$set(
  "public", "add_rect",
  function(x, y, width, height, fill = '#000000', stroke = NULL, linewidth = 1, linetype = 0) {

    paint <- get_path_paint_style(fill, stroke)
    geom  <- glue("{x} {y} {width} {height} re {paint}")
    self$add_geom_stream(geom = geom, fill = fill, stroke = stroke, angle = 0,
                         linewidth = linewidth, linetype = linetype)

    invisible(self)
  }
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name add_line
#' @title Add a straight line
#' @section R6 Usage: $add_line()
#' @description Adds a line to the current document
#'
#' @param pdfdoc PDF document. An R6 object of class \code{PDFDocument}. Created with \code{create_pdfdoc()} or \code{PDFDocument$new()}
#' @param x1,y1,y2,y2 coordinates of line endpoints
#' @param stroke Colour. May be given as hex-colour \code{'#123456'}, RGB vector \code{c(0, 200, 12)}, or RGBA vector e.g. \code{c(0, 156, 255, 255)}
#' @param linewidth line width (pixels)
#' @param linetype Integer from 0 (solid)u to 6 (twodash)
#'
#' @return PDF document with line added
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NULL
PDFDocument$set(
  "public", "add_line",
  function(x1, y1, x2, y2, stroke = '#000000', linewidth = 1, linetype = 0) {

    geom <- glue("{x1} {y1} m {x2} {y2} l s")
    self$add_geom_stream(geom = geom, fill = NULL, stroke = stroke, angle = 0,
                         linewidth = linewidth, linetype = linetype)

    invisible(self)
  }
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name add_polyline
#' @title Add a polyline
#' @section R6 Usage: $add_polyline()
#' @description Adds a line to the current document
#'
#' @param pdfdoc PDF document. An R6 object of class \code{PDFDocument}. Created with \code{create_pdfdoc()} or \code{PDFDocument$new()}
#' @param xs,ys vectors of coordinates of each point along the polyline
#' @param stroke Colour. May be given as hex-colour \code{'#123456'}, RGB vector \code{c(0, 200, 12)}, or RGBA vector e.g. \code{c(0, 156, 255, 255)}
#' @param linewidth line width (pixels)
#' @param linetype Integer from 0 (solid)u to 6 (twodash)
#'
#' @return PDF document with line added
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NULL
PDFDocument$set(
  "public", "add_polyline",
  function(xs, ys, stroke = '#000000', linewidth = 1, linetype = 0) {

    paint <- get_path_paint_style(fill = NULL, stroke)
    lines <- paste(xs[-1], ys[-1], 'l', collapse = ' ')
    geom  <- glue("{xs[1]} {ys[1]} m {lines} S")
    self$add_geom_stream(geom = geom, fill = NULL, stroke = stroke, angle = 0,
                         linewidth = linewidth, linetype = linetype)

    invisible(self)
  }
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name add_polygon
#' @title Add a polygon
#' @section R6 Usage: $add_polygon()
#' @description Adds a polygon to the current document
#'
#' @param pdfdoc PDF document. An R6 object of class \code{PDFDocument}. Created with \code{create_pdfdoc()} or \code{PDFDocument$new()}
#' @param xs,ys vectors of coordinates of points
#' @param fill,stroke Colours. May be given as hex-colour \code{'#123456'}, RGB vector \code{c(0, 200, 12)}, or RGBA vector e.g. \code{c(0, 156, 255, 255)}
#' @param linewidth line width (pixels)
#' @param linetype Integer from 0 (solid)u to 6 (twodash)
#'
#' @return PDF document with polygon added
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NULL
PDFDocument$set(
  "public", "add_polygon",
  function(xs, ys, fill = '#000000', stroke = NULL, linewidth = 1, linetype = 0) {

    stopifnot(length(xs) == length(ys))

    paint <- get_path_paint_style(fill, stroke)
    lines <- paste(xs[-1], ys[-1], 'l', collapse = ' ')
    geom  <- glue("{xs[1]} {ys[1]} m {lines} {paint}")
    self$add_geom_stream(geom = geom, fill = fill, stroke = stroke, angle = 0,
                         linewidth = linewidth, linetype = linetype)

    invisible(self)
  }
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name add_circle
#' @title Add a circle
#' @section R6 Usage: $add_circle()
#' @description Adds a circle to the current document
#' @details
#' There's no direct circle command in PDF, so approximate with 4 beziers
#'
#' @param pdfdoc PDF document. An R6 object of class \code{PDFDocument}. Created with \code{create_pdfdoc()} or \code{PDFDocument$new()}
#' @param x,y,r location and radius of circle
#' @param fill,stroke Colours. May be given as hex-colour \code{'#123456'}, RGB vector \code{c(0, 200, 12)}, or RGBA vector e.g. \code{c(0, 156, 255, 255)}
#' @param linewidth line width (pixels)
#' @param linetype Integer from 0 (solid)u to 6 (twodash)
#'
#' @return PDF document with circle added
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NULL
PDFDocument$set(
  "public", "add_circle",
  function(x, y, r, fill = '#000000', stroke = NULL, linewidth = 1, linetype = 0) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Bezier offset. See:
    # stackoverflow.com/questions/1734745/how-to-create-circle-with-b%C3%A9zier-curves
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    b  <- 0.552284749831 * r

    paint <- get_path_paint_style(fill, stroke)

    geom <- glue("
{x+r} {y} m
{x+r} {y+b}  {x+b} {y+r}  {x}   {y+r} c
{x-b} {y+r}  {x-r} {y+b}  {x-r} {y}   c
{x-r} {y-b}  {x-b} {y-r}  {x}   {y-r} c
{x+b} {y-r}  {x+r} {y-b}  {x+r} {y}   c
{paint}")
    self$add_geom_stream(geom = geom, fill = fill, stroke = stroke, angle = 0,
                         linewidth = linewidth, linetype = linetype)


    invisible(self)
  }
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @name write_pdf
#' @title Write PDF document to file
#' @section R6 Usage: $write_pdf()
#' @description Write the PDF document to the given filename.
#'
#' @param pdfdoc PDF document. An R6 object of class \code{PDFDocument}. Created with \code{create_pdfdoc()} or \code{PDFDocument$new()}
#' @param filename name of output file
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NULL
PDFDocument$set(
  "public", "write_pdf",
  function(filename) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update all cross references
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    self$update_page_contents()
    self$update_gs_references()
    self$update_xref()
    self$update_trailer()


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Combine the Header, Objects, Xref table and Trailer and write to file
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    document <- c(self$header, self$obj, self$xref, self$trailer)
    writeLines(document, filename)
    invisible(self)
  }
)



if (FALSE) {
  library(R6)
  library(glue)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a new document
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pdfdoc <- PDFDocument$new(height = 400, font = 'Courier-Bold')


  pdfdoc$add_text("useless", x = 50, y = 50)
  pdfdoc$add_text("cool", x = 150, y = 150, angle = 30, size = 68, fill = '#ff0000', text_mode = 1, linewidth = 3, linetype = 2)
  pdfdoc$add_rect(x = 10, y = 10, width = 300, height = 80, fill = '#12345620', stroke = '#000000', linewidth = 2)
  pdfdoc$add_circle(x = 100, y = 300, r = 25, stroke = NULL, fill = '#ff000094')
  pdfdoc$add_polygon(xs = c(400, 50, 50, 100), ys = c(50, 400, 200, 300), fill = "#a0a0a030")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write the PDF to file
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pdfdoc$write_pdf("crap.pdf")

}




if (FALSE) {
  library(dplyr)

  pdfdoc <- create_pdfdoc(width = 400, height = 400, font = 'Helvetica-Bold')

  pal <- viridisLite::viridis(80)
  i   <- 0
  for (x in seq(20, 400, 40)) {
    for (y in seq(100, 400, 40)) {
      i <- i + 1
      pdfdoc <- pdfdoc %>% add_circle(x, y, r = 20, fill = pal[i])
    }
  }

  pdfdoc <- pdfdoc %>%
    add_text("Pipe-friendly!", x = 10, y = 20, size = 65, stroke = 'grey50', fill = 'lightblue', text_mode = 2)


  pdfdoc %>% write_pdf("crap.pdf")


}



if (FALSE) {
  pdfdoc <- minipdf::PDFDocument$new(width = 400, height = 400)
  pdfdoc$add_text(text = "hello", x = 200, y = 200)
  pdfdoc$write_pdf("only-text.pdf")
}











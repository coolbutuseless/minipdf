


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Default draw state to use if not specified/over-ridden
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
default_draw_state <- list(
  fontsize  = 12,
  text_mode = 0,
  fill      = '#000000',
  stroke    = '#000000',
  linewidth = 1,
  linetype  = 0,
  clip_rect = NULL
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sanitise colour to 4-element RGBA
#'
#' This uses \code{grDevices::col2rgb()} to handle standard R colours (like 'red')
#' and also hex colours like '#123456ff'
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
#' @noRd
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert a single transform to a string
# e.g. list(rotate = degrees) -> 'ctx.rotate(angleInRadians);'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
transform_to_string <- function(type, value, depth = 0) {
  indent <- create_indent(depth)

  if (type == 'translate') {
    glew("{indent}1 0 0 1 {value[1]} {value[2]} cm")
  } else if (type == 'rotate') {
    cosQ <- round(cos(value * pi/180), 3)
    sinQ <- round(sin(value * pi/180), 3)
    glew("{indent}{cosQ} {sinQ} {-sinQ} {cosQ} 0 0 cm")
  } else if (type == 'scale') {
    glew("{indent}{value[1]} 0 0 {value[2]} 0 0 cm")
  } else if (type == 'custom') {
    glew("{indent}{value[1]} {value[2]} {value[3]} {value[4]} {value[5]} {value[6]} cm")
  } else {
    stop("Unknown transform type: ", type)
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert a list of transforms into a string
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
transforms_to_string <- function(transforms, depth = 0) {
  types <- names(transforms)
  res   <- mapply(transform_to_string, types, transforms, USE.NAMES = FALSE, MoreArgs = list(depth = depth))

  if (length(res) == 0) {
    NULL
  } else {
    paste(res, collapse = "\n")
  }
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Stream Object Base Class
#'
#' @import R6
#' @importFrom utils modifyList
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFStream <- R6::R6Class(
  "PDFStream",

  public = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field attrib TODO
    #' @field transform TODO
    #' @field parent_attrib TODO
    #' @field parent_transform TODO
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    attrib           = NULL,
    transform        = NULL,

    parent_attrib    = NULL,
    parent_transform = NULL,

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Initialize a stream object
    #'
    #' @param ... stream attributes
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(...) {
      self$transform        <- list()
      self$parent_attrib    <- list()
      self$parent_transform <- list()

      self$update(...)
      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Update the attributes of an object.
    #'
    #' e.g. update the x coordinate:
    #' \code{obj$update(x = 12)}
    #'
    #' @param ... named attributes
    #' @param fontsize,text_mode,fill,stroke,linewidth,linetype,clip_rect common
    #'        drawing attributes
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update = function(..., fontsize, text_mode, fill, stroke, linewidth, linetype, clip_rect) {
      args <- find_args(...)
      self$attrib <- modifyList(self$attrib, args, keep.null = TRUE)
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Clone the object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    copy = function() {
      self$clone()
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Convert to full PDF object representation include stream/endstream
    #' and length dict.
    #'
    #' @param obj_idx the index to use for this object in the PDF document
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_object = function(obj_idx) {
      this_stream <- glew("stream\n{self$as_character()}\nendstream")
      chars       <- nchar(self$as_character())
      this_length <- glew("<< /Length {chars} >>")
      res <- glew("{obj_idx} 0 obj\n{this_length}\n{this_stream}\nendobj")
      res
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Cat just the inner stream part of the object when print()ed
    #'
    #' @param ... arguments passed to \code{PDFStream$as_character(...)}
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print = function(...) {
      cat(self$as_character(...))
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Transformations: rotate
    #'
    #' @param degrees angle of rotation
    #' @param x,y rotate around this point. default (0,1)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rotate = function(degrees, x = 0, y = 0) {
      if (x==0 && y==0) {
        transform <- list(rotate = degrees)
      } else {
        transform <- list(translate = c(x, y), rotate = degrees, translate = c(-x, -y))
      }

      self$transform <- append(self$transform, transform)
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Transformations: translate
    #'
    #' @param x,y translation distance
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    translate = function(x, y) {
      transform <- list(translate = c(x, y))

      self$transform <- append(self$transform, transform)
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Transformations: scale
    #'
    #' @param x,y scale factors in x and y directions. If only \code{x} scale
    #'        is given, then this value will also be used for the \code{y} scale factor
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    scale = function(x, y=x) {
      transform <- list(scale = c(x, y))

      self$transform <- append(self$transform, transform)
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Convenience method for changing core attribute: fontsize
    #' @param fontsize fontsize default: 12
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fontsize = function(fontsize = 12) { self$update(fontsize = fontsize ) },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Convenience method for changing core attribute: text_mode
    #' @param text_mode text mode. default 0.  TODO: insert reference to PDF spec
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text_mode = function(text_mode = 0) { self$update(text_mode = text_mode) },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Convenience method for changing core attribute: fill colour
    #' @param fill fill colour. default '#000000'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fill = function(fill = '#000000') { self$update(fill = fill) },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Convenience method for changing core attribute: stroke colour
    #' @param stroke stroke colour. default: '#000000'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    stroke = function(stroke = '#000000') { self$update(stroke = stroke) },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Convenience method for changing core attribute: linewidth
    #' @param linewidth linewidth. default: 1
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    linewidth = function(linewidth = 1) { self$update(linewidth = linewidth) },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Convenience method for changing core attribute: fontsize
    #' @param linetype linetype. default: 0. TODO: Insert reference to PDF spec
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    linetype = function(linetype  = 0) { self$update(linetype = 0) },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Convenience method for changing core attribute: clipping rect
    #' @param x,y,width,height dimensions of current clipping rect
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    clip_rect = function(x, y, width, height) { self$update(clip_rect = c(x, y, width, height)) },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Convenience method for changing core attribute: clip polygon
    #' @param xs,ys coords of clipping polygon
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    clip_polygon = function(xs, ys) { self$update(clip_polygon = list(xs = xs, ys = ys)) },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' convert stream object to character string
    #'
    #' @param ... ignored
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_character = function(...) {

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #By default, assume every stream has its own graphic state unless
      # told otherwise using 'new_graphics_state'
      # i.e. stream is wrapped in q/Q.
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      new_graphics_state <- self$attrib$new_graphics_state %||% TRUE

      push_stack <- if (new_graphics_state) "q" else NULL
      pop_stack  <- if (new_graphics_state) "Q" else NULL

      attrib <- self$get_attrib()

      transform_spec <- self$get_transform_spec()

      paste0(c(
        push_stack,
        transform_spec,
        self$gs_spec,
        self$clipping_spec,
        create_linetype_spec(attrib),
        create_linewidth_spec(attrib),
        create_fill_spec(attrib),
        create_stroke_spec(attrib),
        self$get_geom_spec(),
        pop_stack), collapse = "\n")
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Full transform of all parent objects and this object's transform
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_transform_spec = function() {
      transforms_to_string(c(self$parent_transform, self$transform))
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Normalised full set of attributes including parent attributes if available
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_attrib = function() {
      attrib <- modifyList(default_draw_state, self$parent_attrib, keep.null = TRUE)
      attrib <- modifyList(attrib            , self$attrib       , keep.null = TRUE)
      attrib
    }


    # propagate_state_to_children = function() {
    # }

  ),



  active = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field alphas all alpha channels
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    alphas = function() {
      attrib <- self$get_attrib()
      fill   <- sanitise_colour_to_rgba_vec(attrib$fill)
      stroke <- sanitise_colour_to_rgba_vec(attrib$stroke)

      fill_alpha   <-   fill[4] %||% 1
      stroke_alpha <- stroke[4] %||% 1

      fill_alpha   <- round(fill_alpha  , 2)
      stroke_alpha <- round(stroke_alpha, 2)

      c(fill_alpha, stroke_alpha)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field gs_name current GS name TODO: insert PDF spec reference
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    gs_name = function() {
      alphas <- self$alphas
      paste0("GS", alphas[1], alphas[2])
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field gs_spec current gs_spec
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    gs_spec = function() {
      paste0("/", self$gs_name, " gs")
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field clipping_spec current clipping spec
    #' Use clip_polygon if specified.
    #' Use clip_rect if specified
    #' else NULL
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    clipping_spec = function() {
      attrib <- self$get_attrib()

      # See if user has specified a clipping polygon
      poly <- attrib$clip_polygon
      if (!is.null(poly)) {
        # user has specified a clip polygon using 'xs' and 'ys'
        lines <- paste(poly$xs[-1], poly$ys[-1], 'l', collapse = ' ')
        poly$lines <- lines

        res <- glew(
          "{xs[1]} {ys[1]} m {lines} W n",
          poly
        )
        return(trimws(res))
      }

      # See if user has specified a clipping rect
      rect <- attrib$clip_rect
      if (is.null(rect) || length(rect) != 4) {
        return(NULL)
      }

      x      <- rect[1]
      y      <- rect[2]
      width  <- rect[3]
      height <- rect[4]

      trimws(glew("
{x        } {y         } m
{x + width} {y         } l
{x + width} {y + height} l
{x        } {y + height} l
{x        } {y         } l W n
"
      ))
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field paint_spec path painting style based upon which of fill/stroke are NULL
    #' One of 'n', 's', 'f', 'b'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    paint_spec = function() {
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
      attrib <- self$get_attrib()
      fill   <- attrib$fill
      stroke <- attrib$stroke

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
  )
)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' S3 method for converting PDFStream to character
#'
#' @param x object
#' @param ... unused
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.PDFStream <- function(x, ...) {
  x$as_character(...)
}

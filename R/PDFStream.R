

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
#' PDF Stream Object Base Class
#'
#' @import R6
#' @import glue
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFStream <- R6::R6Class(
  "PDFStream",

  public = list(

    attrib = NULL,
    initialize = function(...) {
      invisible(self)
    },


    update = function(...) {
      self$attrib <- modifyList(self$attrib, list(...))
      invisible(self)
    },

    copy = function() {
      self$clone()
    },

    as_object = function(obj_idx) {
      this_stream <- glue("stream\n{self$stream}\nendstream")
      chars       <- nchar(self$stream)
      this_length <- glue("<< /Length {chars} >>")
      res <- glue("{obj_idx} 0 obj\n{this_length}\n{this_stream}\nendobj")
      res
    },

    print = function() {
      cat(self$stream)
    }

  ),
  active = list(


    stream = function() {
      #By default, assume every stream has its own graphic state.
      # i.e. stream is wrapped in q/Q
      new_graphics_state <- self$attrib$new_graphics_state %||% TRUE

      push_stack <- if (new_graphics_state) "q" else ""
      pop_stack  <- if (new_graphics_state) "Q" else ""

      string <- glue::glue(
        "{push_stack}
{self$transform_spec}
{self$linetype_spec}
{self$linewidth_spec}
/{self$gs_name} gs
{self$clipping_spec}
{self$fill_spec}
{self$stroke_spec}
{self$geom}
{pop_stack}")
      string <- gsub("\n+", "\n", string) # remove blank lines
      trimws(string)
    },

    alphas = function() {
      fill   = sanitise_colour_to_rgba_vec(self$attrib$fill)
      stroke = sanitise_colour_to_rgba_vec(self$attrib$stroke)

      fill_alpha   <-   fill[4] %||% 1
      stroke_alpha <- stroke[4] %||% 1

      fill_alpha   <- round(fill_alpha  , 2)
      stroke_alpha <- round(stroke_alpha, 2)

      c(fill_alpha, stroke_alpha)
    },

    gs_name = function() {
      alphas <- self$alphas
      paste0("GS", alphas[1], alphas[2])
    },

    linetype_spec = function() {
      linetype <- self$attrib$linetype %||% 0
      switch(
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
    },

    linewidth_spec = function() {
      if (is.null(self$attrib$linewidth)) {
        ""
      } else {
        paste(self$attrib$linewidth, 'w')
      }
    },

    fill_spec = function() {
      fill   <- sanitise_colour_to_rgba_vec(self$attrib$fill)
      ifelse(is.null(fill)  , "", glue("{fill[1]} {fill[2]} {fill[3]} rg"))
    },

    stroke_spec = function() {
      stroke <- sanitise_colour_to_rgba_vec(self$attrib$stroke)
      ifelse(is.null(stroke), "", glue("{stroke[1]} {stroke[2]} {stroke[3]} RG"))
    },

    transform_spec = function() {
      if (is.null(self$attrib$transform)) {
        return("")
      }

      paste(self$attrib$transform, "cm", sep = " ", collapse = "\n")

    },

    clipping_spec = function() {
      rect <- self$attrib$clip_rect
      if (is.null(rect) || length(rect) != 4) {
        return("")
      }

      x      <- rect[1]
      y      <- rect[2]
      width  <- rect[3]
      height <- rect[4]

      glue("
{x        } {y         } m
{x + width} {y         } l
{x + width} {y + height} l
{x        } {y + height} l
{x        } {y         } l W n
"
      )
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Get the path painting style based upon which of fill/stroke are NULL
    # Return one of 'n', 's', 'f', 'b'
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
      fill   <- self$attrib$fill
      stroke <- self$attrib$stroke

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

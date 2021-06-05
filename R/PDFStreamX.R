
valid_pdf_fonts <- c(
  'Times-Roman',
  'Times-Bold',
  'Times-Italic',
  'Times-BoldItalic',
  'Helvetica',
  'Helvetica-Bold',
  'Helvetica-Oblique',
  'Helvetica-BoldOblique',
  'Courier',
  'Courier-Bold',
  'Courier-Oblique',
  'Courier-BoldOblique',
  'Symbol',
  'ZapfDingbats'
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Text stream object creator
#'
#' This is the R6 class representing the text stream object.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFText <- R6::R6Class(
  "PDFText", inherit = PDFStream,

  public = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a text
    #'
    #' @param text text
    #' @param x,y location
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(text, x, y, ...) {
      self$attrib <- list(
        text      = text,
        x         = x,
        y         = y
      )

      super$initialize(...)
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Fetch the character representation of this geometry
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_geom_spec = function() {
      attrib <- self$get_attrib()
      trimws(glew(
        "
BT
    /F1 {fontsize} Tf
    {x} {y} Td
    {text_mode} Tr
    ({text}) Tj
ET"
      ,
attrib))
    }
  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Line stream object creator
#'
#' This is the R6 class representing the line stream object.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFLine <- R6::R6Class(
  "PDFLine", inherit = PDFStream,

  public = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a line
    #'
    #'@param x1,y1,x2,y2 line start/end coordinates
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(x1, y1, x2, y2, ...) {

      self$attrib <- list(
        x1 = x1,
        y1 = y1,
        x2 = x2,
        y2 = y2
      )

      super$initialize(...)
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Fetch the character representation of this geometry
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_geom_spec = function() {
      attrib <- self$get_attrib()
      glew(
        "{x1} {y1} m {x2} {y2} l s",
        attrib
      )
    }
  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Rect stream object creator
#'
#' This is the R6 class representing the rect stream object.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFRect <- R6::R6Class(
  "PDFRect", inherit = PDFStream,

  public = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a rectangle
    #'
    #' @param x,y,width,height specificaiton of rectangle extents
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(x, y, width, height, ...) {

      self$attrib <- list(
        x      = x,
        y      = y,
        width  = width,
        height = height
      )

      super$initialize(...)
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Fetch the character representation of this geometry
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_geom_spec = function() {
      attrib <- self$get_attrib()
      attrib$paint <- self$paint_spec
      glew(
        "{x} {y} {width} {height} re {paint}",
        attrib
      )
    }
  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Polyline stream object creator
#'
#' This is the R6 class representing the polyline stream object.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFPolyline <- R6::R6Class(
  "PDFPolyline", inherit = PDFStream,

  public = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a polyline
    #'
    #' @param xs,ys numeric vectors of x, ycoordinates along polyline
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(xs, ys, ...) {

      stopifnot(length(xs) > 0, length(xs) == length(ys), is.numeric(xs), is.numeric(ys))

      self$attrib <- list(
        xs        = xs,
        ys        = ys
      )

      super$initialize(...)
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Fetch the character representation of this geometry
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_geom_spec = function() {
      attrib <- self$get_attrib()
      lines <- paste(attrib$xs[-1], attrib$ys[-1], 'l', collapse = ' ')
      attrib$lines <- lines
      attrib$paint <- self$paint_spec

      glew(
        "{xs[1]} {ys[1]} m {lines} S",
        attrib
      )
    }
  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Polygon stream object creator
#'
#' This is the R6 class representing the polygon stream object.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFPolygon <- R6::R6Class(
  "PDFPolygon", inherit = PDFStream,

  public = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a polygon
    #'
    #' @param xs,ys coordinates of polygon vertices
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(xs, ys, ...) {

      self$attrib <- list(
        xs        = xs,
        ys        = ys
      )

      super$initialize(...)
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Fetch the character representation of this geometry
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_geom_spec = function() {
      attrib <- self$get_attrib()

      lines <- paste(attrib$xs[-1], attrib$ys[-1], 'l', collapse = ' ')
      attrib$lines <- lines
      attrib$paint <- self$paint_spec

      glew(
        "{xs[1]} {ys[1]} m {lines} {paint}",
        attrib
      )
    }
  )
)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Circle stream object creator
#'
#' This is the R6 class representing the circle stream object.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFCircle <- R6::R6Class(
  "PDFCircle", inherit = PDFStream,

  public = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a circle
    #'
    #' @param x,y centre of circle
    #' @param r radius
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(x, y, r, ...) {

      self$attrib <- list(
        x         = x,
        y         = y,
        r         = r
      )

      super$initialize(...)
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Fetch the character representation of this geometry
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_geom_spec = function() {
      attrib <- self$get_attrib()
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Bezier offset. See:
      # stackoverflow.com/questions/1734745/how-to-create-circle-with-b%C3%A9zier-curves
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      attrib$b     <- 0.552284749831 * attrib$r

      attrib$paint <- self$paint_spec

      trimws(glew(
        "
{x+r} {y} m
{x+r} {y+b}  {x+b} {y+r}  {x}   {y+r} c
{x-b} {y+r}  {x-r} {y+b}  {x-r} {y}   c
{x-r} {y-b}  {x-b} {y-r}  {x}   {y-r} c
{x+b} {y-r}  {x+r} {y-b}  {x+r} {y}   c
{paint}",
  attrib))
    }
  )
)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF ClipRect stream object creator
#'
#' This is the R6 class representing the ClipRect stream object.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFClipRect <- R6::R6Class(
  "PDFClipRect", inherit = PDFStream,

  public = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a clipping rectangle
    #'
    #' @param x,y,width,height rectangle definition
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(x, y, width, height, ...) {

      self$attrib <- list(
        x      = x,
        y      = y,
        width  = width,
        height = height,
        new_graphics_state = FALSE
      )

      super$initialize(...)
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Fetch the character representation of this geometry
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_geom_spec = function() {
      attrib <- self$get_attrib()
      trimws(glew(
        "
{x        } {y         } m
{x + width} {y         } l
{x + width} {y + height} l
{x        } {y + height} l
{x        } {y         } l W n
", attrib
      ))
    }
  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF ClipPolygon stream object creator
#'
#' This is the R6 class representing the ClipPolygon stream object.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFClipPolygon <- R6::R6Class(
  "PDFClipPolygon", inherit = PDFStream,

  public = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a cliping polygon
    #'
    #' @param xs,ys coordinates of polygon vertices
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(xs, ys, ...) {

      self$attrib <- list(
        xs        = xs,
        ys        = ys,
        new_graphics_state = FALSE
      )

      super$initialize(...)
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Fetch the character representation of this geometry
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_geom_spec = function() {
      attrib <- self$get_attrib()
      lines <- paste(attrib$xs[-1], attrib$ys[-1], 'l', collapse = ' ')
      attrib$lines <- lines

      glew(
        "{xs[1]} {ys[1]} m {lines} W n",
        attrib
      )
    }
  )
)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Custom stream object creator
#'
#' This is the R6 class representing the custom stream object.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFCustom <- R6::R6Class(
  "PDFCustom", inherit = PDFStream,

  public = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a custom object with text
    #'
    #' @param text text string
    #' @param new_graphics_state Should the object be drawn in its own local
    #'        graphics state? default: TRUE
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(text, new_graphics_state = TRUE, ...) {

      self$attrib <- list(
        text = text,
        new_graphics_state = new_graphics_state
      )

      super$initialize(...)
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Update the attributes of this object
    #'
    #' @param text text
    #' @param ... other atttributes
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update = function(text, ...) {
      self$attrib$text <- text
      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Fetch the character representation of this geometry
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_geom_spec = function() {
      attrib <- self$get_attrib()
      as.character(attrib$text)
    }
  )
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Helper for building PDF Stream Objects e.g. Rect, Lines
#'
#'For documentation, see manual for the related R6 function e.g. `PDFRect`
#'
#' @examples
#' \dontrun{
#' ptag$rect(x = 0, y = 0, width = 100, height = 100)
#' ptag$circle(x = 100, y = 100, r = 10, fill = 'black')
#' }
#'
#' @seealso PDFText PDFRect PDFLine PDFPolyline PDFPolygon PDFCircle PDFCustom
#' PDFClipRect PDFClipPolygon
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ptag <- list(
  rect         = function(x, y, width, height            , ...) {do.call(PDFRect$new       , find_args(...))},
  line         = function(x1, y1, x2, y2                 , ...) {do.call(PDFLine$new       , find_args(...))},
  text         = function(text, x, y, fontsize, text_mode, ...) {do.call(PDFText$new       , find_args(...))},
  circle       = function(x, y, r                        , ...) {do.call(PDFCircle$new     , find_args(...))},
  polygon      = function(xs, ys                         , ...) {do.call(PDFPolygon$new    , find_args(...))},
  polyline     = function(xs, ys                         , ...) {do.call(PDFPolyline$new   , find_args(...))},
  custom       = function(text                           , ...) {do.call(PDFCustom$new     , find_args(...))}
  # clip_rect    = function(x, y, width, height            , ...) {do.call(PDFClipRect$new   , find_args(...))},
  # clip_polygon = function(x, y, width, height            , ...) {do.call(PDFClipPolygon$new, find_args(...))},
)




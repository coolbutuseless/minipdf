
valid_pdf_fonts <- c('Times-Roman', 'Times-Bold', 'Times-Italic', 'Times-BoldItalic',
               'Helvetica', 'Helvetica-Bold', 'Helvetica-Oblique', 'Helvetica-BoldOblique',
               'Courier'  , 'Courier-Bold'  , 'Courier-Oblique'  , 'Courier-BoldOblique'  ,
               'Symbol', 'ZapfDingbats')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Text stream object creator
#'
#' This is the R6 class representing the text stream object.
#'
#'
#'\itemize{
#' \item{text text string for PDFText}
#' \item{x,y coordinates}
#' \item{... extra arguments specifying initial state e.g. 'fill'}
#'}
#'
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFText <- R6::R6Class(
  "PDFText", inherit = PDFStream,

  public = list(

    initialize = function(text, x, y, ...) {
      self$attrib <- list(
        text      = text,
        x         = x,
        y         = y
      )

      super$initialize(...)
      invisible(self)
    }
  ),



  active = list(
    geom = function() {
      glue::glue_data(
        self$attrib,
        "
BT
  /F1 {fontsize} Tf
  {x} {y} Td
  {text_mode} Tr
  ({text}) Tj
ET"
      )
    }
  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Line stream object creator
#'
#' This is the R6 class representing the line stream object.
#'
#'
#'\itemize{
#' \item{x1,y1,x2,y2 pair of coordinates}
#' \item{... extra arguments specifying initial state e.g. 'fill'}
#'}
#'
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFLine <- R6::R6Class(
  "PDFLine", inherit = PDFStream,

  public = list(

    initialize = function(x1, y1, x2, y2, ...) {

      self$attrib <- list(
        x1 = x1,
        y1 = y1,
        x2 = x2,
        y2 = y2
      )

      super$initialize(...)
      invisible(self)
    }
  ),



  active = list(
    geom = function() {
      glue::glue_data(
        self$attrib,
        "{x1} {y1} m {x2} {y2} l s"
      )
    }
  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Rect stream object creator
#'
#' This is the R6 class representing the rect stream object.
#'
#'
#'\itemize{
#' \item{x,y coordinates}
#' \item{width,height rectangle width and height}
#' \item{... extra arguments specifying initial state e.g. 'fill'}
#'}
#'
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFRect <- R6::R6Class(
  "PDFRect", inherit = PDFStream,

  public = list(

    initialize = function(x, y, width, height, ...) {

      self$attrib <- list(
        x      = x,
        y      = y,
        width  = width,
        height = height
      )

      super$initialize(...)
      invisible(self)
    }
  ),

  active = list(
    geom = function() {
      self$attrib$paint <- self$paint_spec
      glue::glue_data(
        self$attrib,
        "{x} {y} {width} {height} re {paint}"
      )
    }
  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Polyline stream object creator
#'
#' This is the R6 class representing the polyline stream object.
#'
#'
#'\itemize{
#' \item{x,y coordinates}
#' \item{... extra arguments specifying initial state e.g. 'fill'}
#'}
#'
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFPolyline <- R6::R6Class(
  "PDFPolyline", inherit = PDFStream,

  public = list(

    initialize = function(xs, ys, ...) {

      self$attrib <- list(
        xs        = xs,
        ys        = ys
      )

      super$initialize(...)
      invisible(self)
    }
  ),

  active = list(
    geom = function() {
      lines <- paste(self$attrib$xs[-1], self$attrib$ys[-1], 'l', collapse = ' ')
      self$attrib$lines <- lines
      self$attrib$paint <- self$paint_spec

      glue::glue_data(
        self$attrib,
        "{xs[1]} {ys[1]} m {lines} S"
      )
    }
  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Polygon stream object creator
#'
#' This is the R6 class representing the polygon stream object.
#'
#'
#'\itemize{
#' \item{xs,ys vectors of x and y coordinates}
#' \item{... extra arguments specifying initial state e.g. 'fill'}
#'}
#'
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFPolygon <- R6::R6Class(
  "PDFPolygon", inherit = PDFStream,

  public = list(

    initialize = function(xs, ys, ...) {

      self$attrib <- list(
        xs        = xs,
        ys        = ys
      )

      super$initialize(...)
      invisible(self)
    }
  ),

  active = list(
    geom = function() {
      lines <- paste(self$attrib$xs[-1], self$attrib$ys[-1], 'l', collapse = ' ')
      self$attrib$lines <- lines
      self$attrib$paint <- self$paint_spec

      glue::glue_data(
        self$attrib,
        "{xs[1]} {ys[1]} m {lines} {paint}"
      )
    }
  )
)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Circle stream object creator
#'
#' This is the R6 class representing the circle stream object.
#'
#'
#'\itemize{
#' \item{x,y coordinates}
#' \item{r radius of circle}
#' \item{... extra arguments specifying initial state e.g. 'fill'}
#'}
#'
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFCircle <- R6::R6Class(
  "PDFCircle", inherit = PDFStream,

  public = list(

    initialize = function(x, y, r, ...) {

      self$attrib <- list(
        x         = x,
        y         = y,
        r         = r
      )

      super$initialize(...)
      invisible(self)
    }
  ),

  active = list(
    geom = function() {
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Bezier offset. See:
      # stackoverflow.com/questions/1734745/how-to-create-circle-with-b%C3%A9zier-curves
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self$attrib$b     <- 0.552284749831 * self$attrib$r

      self$attrib$paint <- self$paint_spec

      glue::glue_data(
        self$attrib,
        "
{x+r} {y} m
{x+r} {y+b}  {x+b} {y+r}  {x}   {y+r} c
{x-b} {y+r}  {x-r} {y+b}  {x-r} {y}   c
{x-r} {y-b}  {x-b} {y-r}  {x}   {y-r} c
{x+b} {y-r}  {x+r} {y-b}  {x+r} {y}   c
{paint}"
      )
    }
  )
)






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF ClipRect stream object creator
#'
#' This is the R6 class representing the ClipRect stream object.
#'
#'
#'\itemize{
#' \item{x,y coordinates}
#' \item{width,height rectangle width and height}
#' \item{... extra arguments specifying initial state e.g. 'fill'}
#'}
#'
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFClipRect <- R6::R6Class(
  "PDFClipRect", inherit = PDFStream,

  public = list(

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
    }
  ),


  # 'W' to set this as a clipping path.
  # 'n' for "don't actually draw the path"
  active = list(
    geom = function() {
      glue::glue_data(
        self$attrib,
        "
{x        } {y         } m
{x + width} {y         } l
{x + width} {y + height} l
{x        } {y + height} l
{x        } {y         } l W n
"
      )
    }
  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF ClipPolygon stream object creator
#'
#' This is the R6 class representing the ClipPolygon stream object.
#'
#'
#'\itemize{
#' \item{xs,ys vectors of x and y coordinates}
#' \item{... extra arguments specifying initial state e.g. 'fill'}
#'}
#'
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFClipPolygon <- R6::R6Class(
  "PDFClipPolygon", inherit = PDFStream,

  public = list(

    initialize = function(xs, ys, ...) {

      self$attrib <- list(
        xs        = xs,
        ys        = ys,
        new_graphics_state = FALSE
      )

      super$initialize(...)
      invisible(self)
    }
  ),

  active = list(
    geom = function() {
      lines <- paste(self$attrib$xs[-1], self$attrib$ys[-1], 'l', collapse = ' ')
      self$attrib$lines <- lines

      glue::glue_data(
        self$attrib,
        "{xs[1]} {ys[1]} m {lines} W n"
      )
    }
  )
)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Custom stream object creator
#'
#' This is the R6 class representing the custom stream object.
#'
#'
#'\itemize{
#' \item{text text string for PDFText}
#' \item{new_graphics_state Should the object be drawn in its own local graphics state? default: TRUE}
#' \item{... extra arguments specifying initial state e.g. 'fill'}
#'}
#'
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFCustom <- R6::R6Class(
  "PDFCustom", inherit = PDFStream,

  public = list(

    initialize = function(text, new_graphics_state = TRUE, ...) {

      self$attrib <- list(
        text = text,
        new_graphics_state = new_graphics_state
      )

      super$initialize(...)
      invisible(self)
    },

    update = function(text, ...) {
      self$attrib$text <- text
      invisible(self)
    }
  ),


  # 'W' to set this as a clipping path.
  # 'n' for "don't actually draw the path"
  active = list(
    geom = function() {
      as.character(self$attrib$text)
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
#' stream$rect(x = 0, y = 0, width = 100, height = 100)
#' stream$circle(x = 100, y = 100, r = 10, fill = 'black')
#' }
#'
#' @seealso PDFText PDFRect PDFLine PDFPolyline PDFPolygon PDFCircle PDFCustom
#' PDFClipRect PDFClipPolygon
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stream <- list(
  rect         = function(x, y, width, height            , fill='#000000',stroke='#000000', linewidth=1, linetype=0, ...) {do.call(PDFRect$new       , find_args(...))},
  line         = function(x1, y1, x2, y2                                 ,stroke='#000000', linewidth=1, linetype=0, ...) {do.call(PDFLine$new       , find_args(...))},
  text         = function(text, x, y, fontsize, text_mode, fill='#000000',stroke='#000000', linewidth=1, linetype=0, ...) {do.call(PDFText$new       , find_args(...))},
  circle       = function(x, y, r                        , fill='#000000',stroke='#000000', linewidth=1, linetype=0, ...) {do.call(PDFCircle$new     , find_args(...))},
  polygon      = function(xs, ys                         , fill='#000000',stroke='#000000', linewidth=1, linetype=0, ...) {do.call(PDFPolygon$new    , find_args(...))},
  polyline     = function(xs, ys                         , fill='#000000',stroke='#000000', linewidth=1, linetype=0, ...) {do.call(PDFPolyline$new   , find_args(...))},
  clip_rect    = function(x, y, width, height            , fill='#000000',stroke='#000000', linewidth=1, linetype=0, ...) {do.call(PDFClipRect$new   , find_args(...))},
  clip_polygon = function(x, y, width, height            , fill='#000000',stroke='#000000', linewidth=1, linetype=0, ...) {do.call(PDFClipPolygon$new, find_args(...))},
  custom       = function(text                           , fill='#000000',stroke='#000000', linewidth=1, linetype=0, ...) {do.call(PDFCustom$new     , find_args(...))}
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add public methods to PDFDocument to create and add Stream objects in an R6ish way
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFDocument$set("public", "text"        , function(text, x, y, fontsize, text_mode, fill='#000000', stroke='#000000', linewidth=1, linetype=0, clip_rect=NULL, ...) {obj <- do.call(PDFText$new       , find_args(...)); self$append(obj); invisible(obj)})
PDFDocument$set("public", "rect"        , function(x, y, width, height            , fill='#000000', stroke='#000000', linewidth=1, linetype=0, clip_rect=NULL, ...) {obj <- do.call(PDFRect$new       , find_args(...)); self$append(obj); invisible(obj)})
PDFDocument$set("public", "line"        , function(x1, y1, x2, y2                                 , stroke='#000000', linewidth=1, linetype=0, clip_rect=NULL, ...) {obj <- do.call(PDFLine$new       , find_args(...)); self$append(obj); invisible(obj)})
PDFDocument$set("public", "circle"      , function(x, y, r                        , fill='#000000', stroke='#000000', linewidth=1, linetype=0, clip_rect=NULL, ...) {obj <- do.call(PDFCircle$new     , find_args(...)); self$append(obj); invisible(obj)})
PDFDocument$set("public", "polygon"     , function(xs, ys                         , fill='#000000', stroke='#000000', linewidth=1, linetype=0, clip_rect=NULL, ...) {obj <- do.call(PDFPolygon$new    , find_args(...)); self$append(obj); invisible(obj)})
PDFDocument$set("public", "polyline"    , function(xs, ys                         , fill='#000000', stroke='#000000', linewidth=1, linetype=0, clip_rect=NULL, ...) {obj <- do.call(PDFPolyline$new   , find_args(...)); self$append(obj); invisible(obj)})
PDFDocument$set("public", "clip_rect"   , function(x, y, width, height            , fill='#000000', stroke='#000000', linewidth=1, linetype=0, clip_rect=NULL, ...) {obj <- do.call(PDFClipRect$new   , find_args(...)); self$append(obj); invisible(obj)})
PDFDocument$set("public", "clip_polygon", function(xs, ys                         , fill='#000000', stroke='#000000', linewidth=1, linetype=0, clip_rect=NULL, ...) {obj <- do.call(PDFClipPolygon$new, find_args(...)); self$append(obj); invisible(obj)})
PDFDocument$set("public", "custom"      , function(text                           , fill='#000000', stroke='#000000', linewidth=1, linetype=0, clip_rect=NULL, new_graphics_state = TRUE, ...) {obj <- do.call(PDFCustom$new  , find_args(...)); self$append(obj); invisible(obj)})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add public method to PDFDocument to create and add Dict objects in an R6ish way
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFDocument$set("public", "dict"  , function(...) {obj <- PDFDict$new(...); self$append(obj); invisible(obj)})





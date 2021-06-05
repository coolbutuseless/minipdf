

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PDF Document creator
#'
#' \itemize{
#' \item{\code{fontname} - default: Helvetica. Choose one of the standard PDF
#' fonts: Times-Roman, Times-Bold,
#' Times-Italic, Times-BoldItalic, Helvetica, Helvetica-Bold, Helvetica-Oblique,
#' Helvetica-BoldOblique, Courier, Courier-Bold, Courier-Oblique, Courier-BoldOblique,
#' Symbol, ZapfDingbats}
#' \item{\code{width.height} document dimensions. Default 400x400}
#' \item{\code{version} Default: 1.2.  Doesn't do anything other than set the string at the
#'        beginning of the PDF document.}
#' \item{\code{...} PDF objects}
#' }
#'
#'
#' @import R6
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFDocument <- R6::R6Class(
  "PDFDocument",


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' @field version PDF spec version
  #' @field width,height document width and height
  #' @field user_objects User elements added to the document
  #' @field setup_objects standard PDF header objects not directly defined
  #'        by the user
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public = list(

    version       = NULL,
    width         = NULL,
    height        = NULL,
    user_objects  = NULL,
    setup_objects = NULL,


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Initialize a new PDF document
    #'
    #' Configure the setup objects and initialise the user_objects
    #' The setup objects are those PDF objects needed to initialise the page
    #' at the most basic level i.e.
    #' \itemize{
    #' \item{set up a catalog}
    #' \item{set up a list of pages}
    #' \item{set up font information.}
    #' \item{set up the list of page contents (which will get updated with each new object)}
    #' \item{keep track of GraphicStates used by various objects}
    #' }
    #'
    #' @param ... user objects to add during initialisation
    #' @param width,height dimensions of PDF document
    #' @param fontname default font for document (Default: 'Helvetica')
    #' @param version PDF specifcation version. Default: 1.2
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(..., width = 400, height = 400, fontname = 'Helvetica', version = 1.2) {

      self$width   <- width
      self$height  <- height
      self$version <- glew("%PDF-{version}")

      self$setup_objects = list(
        dict(Type = '/Catalog', Pages = "2 0 R"),
        dict(Type = '/Pages', Kids = "[3 0 R]", Count = 1),
        dict(
          Type      = '/Page',
          Parent    = "2 0 R",
          Resources = "4 0 R",
          MediaBox  = glew("[0 0 {width} {height}]"),
          Contents  = "[6 0 R]"
        ),
        dict(
          Font      = dict(F1 = "5 0 R"),
          ExtGState = dict(GS11 = dict(ca = 1, CA = 1))
        ),
        dict(Type = '/Font', Subtype = "/Type1", BaseFont = paste0("/", fontname))
      )

      self$user_objects <- list(...)

      # Always add 1 default user object to the document.
      # this is just a clipping path that encompasses the entire canvas and
      # has no real effect other than to make the document have *something*
      # in it.
      clip = PDFClipRect$new(x = 0, y = 0, width = self$width, height = self$height)
      self$append(clip)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Append user-defined objects to document
    #' @param ... objects
    #' @param position where should the objects be inserted in the user object
    #'        list?  Default: NULL means to append at end of object list.
    #'        A value of "1" means to insert at start of object list.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    append = function(..., position = NULL) {
      pdf_objects <- list(...)
      if (is.null(position)) {
        self$user_objects <- append(self$user_objects, pdf_objects)
      } else {
        self$user_objects <- append(self$user_objects, pdf_objects, after = position - 1)
      }

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Replace an object at the given position
    #'
    #' @param pdf_object User defined PDF object e.g. TODO
    #' @param position numeric index in object list
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    replace = function(pdf_object, position) {
      if (!is.numeric(position) || length(position) != 1 ||
        position < 1 || position > length(self$user_objects)) {
        stop("Position not valid")
      }
      self$user_objects[[position]] <- pdf_object

      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Delete the object at the given position
    #'
    #' @param position numeric index in object list
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    delete = function(position) {
      if (!is.numeric(position) || length(position) != 1 ||
          position < 1 || position > length(self$user_objects)) {
        stop("Position not valid")
      }
      self$user_objects[position] <- NULL
      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Character reprsetntatino of PDF document as a single string
    #'
    #' @param ... ignored
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_character = function(...) {
      private$update_page_contents()
      private$update_graphics_state_dict()
      paste(self$version, self$body, self$xref, self$trailer, sep = "\n")
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Print
    #'
    #' @param ... ignored
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print = function(...) {
      cat(self$as_character(...))
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Save to PDF file
    #'
    #' @param filename output filename
    #' @param ... ignored
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    save = function(filename, ...) {
      writeLines(self$as_character(), filename)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Deep copy of this document
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    copy = function() {
      self$clone(deep = TRUE)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Show the PDF in the defined R viewer
    #'
    #' @param viewer which viewer to use
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    show = function(viewer = getOption("viewer", utils::browseURL)) {
      temp_dir <- tempfile("viewpdf")
      dir.create(temp_dir)
      temp_pdf <- file.path(temp_dir, "temp.pdf")
      self$save(temp_pdf)

      if (!is.null(viewer)) {
        viewer(temp_pdf)
      } else {
        warning("No viewer available.")
      }
      invisible(temp_pdf)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a text
    #'
    #' @param text text
    #' @param x,y location
    #' @param fontsize,text_mode text settings [optional]
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text = function(text, x, y, fontsize, text_mode, ...) {
      obj <- do.call(PDFText$new, find_args(...))
      self$append(obj)
      invisible(obj)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a rectangle
    #'
    #' @param x,y,width,height specificaiton of rectangle extents
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rect = function(x, y, width, height, ...) {
      obj <- do.call(PDFRect$new, find_args(...))
      self$append(obj)
      invisible(obj)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a line
    #'
    #'@param x1,y1,x2,y2 line start/end coordinates
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    line = function(x1, y1, x2, y2, ...) {
      obj <- do.call(PDFLine$new, find_args(...))
      self$append(obj)
      invisible(obj)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a circle
    #'
    #' @param x,y centre of circle
    #' @param r radius
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    circle = function(x, y, r, ...) {
      obj <- do.call(PDFCircle$new, find_args(...))
      self$append(obj)
      invisible(obj)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a polygon
    #'
    #' @param xs,ys coordinates of polygon vertices
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    polygon = function(xs, ys, ...) {
      obj <- do.call(PDFPolygon$new, find_args(...))
      self$append(obj)
      invisible(obj)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a polyline
    #'
    #' @param xs,ys numeric vectors of x, ycoordinates along polyline
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    polyline = function(xs, ys, ...) {
      obj <- do.call(PDFPolyline$new, find_args(...))
      self$append(obj)
      invisible(obj)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize a stream object representing a custom object with text
    #'
    #' @param text text string
    #' @param new_graphics_state Should the object be drawn in its own local
    #'        graphics state? default: TRUE
    #' @param ... initial named attributes of this object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    custom = function(text, new_graphics_state = TRUE, ...) {
      obj <- do.call(PDFCustom$new, find_args(...))
      self$append(obj)
      invisible(obj)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Initialize a new PDF Dict
    #'
    #' @param ... named objects to add to dictionary
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dict = function(...) {
      obj <- PDFDict$new(...)
      self$append(obj)
      invisible(obj)
    }
  ),

  private = list(
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # When called with `$clone(deep = TRUE)`, the 'deep_clone' function is
    # called for every name/value pair in the object.
    # See: https://r6.r-lib.org/articles/Introduction.html
    # Need special handling for:
    #   - 'user_objects' and 'setup_objects' are lists of R6 objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    deep_clone = function(name, value) {
      if (name %in% c('user_objects', 'setup_objects')) {
        lapply(value, function(x) {if (inherits(x, "R6")) x$clone(deep = TRUE) else x})
      } else {
        value
      }
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The 3rd setup object should contain a reference to every single object
    # on this page.  Add a reference to every user object to this dict
    #. i.e. /Contents [6 0 R 7 0 R    etc   ]
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_page_contents = function() {
      pages        <- seq_along(self$user_objects) + length(self$setup_objects)
      pages_string <- paste(pages, "0 R", sep = " ", collapse = " ")
      pages_string <- paste0("[", pages_string, "]")
      self$setup_objects[[3]]$update(Contents = pages_string)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Find out all the graphics states required for all the child objects.
    # add these objects to the 'setup_objects'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    update_graphics_state_dict = function() {
      gs <- lapply(self$user_objects, function(x) {x$alphas})
      gs <- unique(gs)
      gs <- Filter(Negate(is.null), gs)


      # Just creating the character version of the dict, rather than a full
      # on PDFDict object, however it might be necessary to go this way if
      # there are more vars in the graphics-state than just the alphas
      # ca = fill alpha,  CA = stroke alpha  e.g. /GS1.13 <</ca 1 /CA 0.13>>
      gs_dicts <- vapply(gs, function(x) {glew("/GS{x[1]}{x[2]} <</ca {x[1]} /CA {x[2]}>>")}, character(1))
      gs_dict  <- paste(gs_dicts, collapse = " ")
      gs_dict  <- paste("<<", gs_dict, ">>")
      # Overwrite the ExtGSState to encompass all the graphics states we
      # get from the user objects
      self$setup_objects[[4]]$update(ExtGState = gs_dict)
    }
  ),


  active = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field all_objects List containing all objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    all_objects = function() {
      c(self$setup_objects, self$user_objects)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field objs All strings for all objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    objs = function() {
      n <- length(self$all_objects)
      obj_string <- character(n)
      for (i in seq(n)) {
        obj_string[i] <- paste0(self$all_objects[[i]]$as_object(i), "\n")
      }
      obj_string
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field header The header for this document
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    header = function() {
      self$version
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field body The body is just the collection of all objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    body = function() {
      trimws(paste(self$objs, collapse = ""))
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field xref Update a xref table based upon the current objects
    #'
    #' and the offsets to the start of each object
    #' Each xref entry must be exactly 20bytes (include CRLF).  Since I'm only
    #' using CR, I need to add an extra space at the end of each xref entry
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xref = function() {
      all_offsets    <- self$offsets
      object_offsets <- head(all_offsets, -1L)
      xref <- c(
        "xref",
        glew("0 {length(self$all_objects) + 1L}"),
        "0000000000 65535 f ",
        sprintf("%010i 00000 n ", object_offsets)
      )

      paste(xref, collapse = "\n")
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field offsets For the XREF table, need to know the offset to each object from start of document
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    offsets = function() {
      cumsum(nchar(c(self$header, self$objs)))
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field trailer The trailer indicates the start of the XREF table and total length
    #' of all objects it represents (including the dummy/root object)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    trailer = function() {
      start_xref <- tail(self$offsets, 1) # byte index of start of xref table

      trailer <- c(
        glew("trailer <</Size {length(self$all_objects) + 1}/Root 1 0 R>>"),
        "startxref",
        start_xref,
        "%%EOF"
      )

      paste(trailer, collapse = "\n")

    }
  )
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Wrapper to help create a PDFDocument object
#'
#' @param width,height document dimensions. Default 400x400
#' @param fontname default: Helvetica. Choose one of the standard PDF
#' fonts: Times-Roman, Times-Bold,
#' Times-Italic, Times-BoldItalic, Helvetica, Helvetica-Bold, Helvetica-Oblique,
#' Helvetica-BoldOblique, Courier, Courier-Bold, Courier-Oblique, Courier-BoldOblique,
#' Symbol, ZapfDingbats
#' @param version Default: 1.2.  Doesn't do anything other than set the string at the
#'        beginning of the PDF document.
#' @param ... PDF objects
#'
#' @return PDFDocument R6 object
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_doc <- function(..., width = 400, height = 400, fontname = 'Helvetica', version = 1.2) {
  PDFDocument$new(..., width = width, height = height, fontname = fontname, version = version)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' S3 method for converting PDFDict to character
#'
#' @param x PDFDocument object
#' @param ... unused
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.PDFDocument <- function(x, ...) {
  x$as_character(...)
}



if (FALSE) {
  doc <- PDFDocument$new()
  # doc$append(PDFCircle$new(x = 150, y= 150, r = 200, fill = 'red', stroke = 'blue', linewidth = 6))
  # doc$append(PDFPolyline$new(xs = c(0, 0, 150), ys = c(0, 150, 150)))
  # doc$append(PDFPolygon$new(xs = c(0, 0, 150), ys = c(0, 150, 150)))
  # doc$append(PDFRect$new(150, 50, 300, 200))
  # doc$append(PDFLine$new(50, 50, 400, 350))

  pdftext <- PDFText$new("Hello", 200, 200, fill = '#00000080')$
    rotate(10)$
    translate(0, 0)

  doc$append(pdftext)

  doc$text("goodbye222", 300, 300, fill = 'blue')


#   custom <- PDFCustom$new(text = "BT
#   /F1 12 Tf
#   220 220 Td
#   0 Tr
#   (More Text) Tj
# ET", stroke = 'green')
#   doc$append(custom)


  doc$save("crap.pdf")
  doc
}


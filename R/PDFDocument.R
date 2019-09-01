

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
#' @import glue
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PDFDocument <- R6::R6Class(
  "PDFDocument",


  public = list(

    version       = NULL,
    width         = NULL,
    height        = NULL,
    user_objects  = NULL,
    setup_objects = NULL,


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Configure the setup objects and initialise the user_objects
    # The setup objects are those PDF objects needed to initialise the page
    # at the most basic level i.e.
    #   - set up a catalog
    #   - set up a list of pages
    #   - set up font information.
    #   - set up the list of page contents (which will get updated with each new object)
    #   - keep track of GraphicStates used by various objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(..., width = 400, height = 400, fontname = 'Helvetica', version = 1.2) {

      self$width   <- width
      self$height  <- height
      self$version <- glue("%PDF-{version}")

      self$setup_objects = list(
        dict(Type = '/Catalog', Pages = "2 0 R"),
        dict(Type = '/Pages', Kids = "[3 0 R]", Count = 1),
        dict(
          Type      = '/Page',
          Parent    = "2 0 R",
          Resources = "4 0 R",
          MediaBox  = glue("[0 0 {width} {height}]"),
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
      self$reset_clip_rect()

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Set the clipping rect to be the entire document
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    reset_clip_rect = function() {
      clip = PDFClipRect$new(x = 0, y = 0, width = self$width, height = self$height)
      self$append(clip)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Append objects into the list of all objects in this document
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
    # Replace an object at the given position
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    replace = function(pdf_object, position) {
      self$user_objects[[position]] <- pdf_object

      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Delete the object at the given position
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    delete = function(position) {
      self$user_objects[position] <- NULL
      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Character reprsetntatino of PDF document as a single string
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    as_character = function(...) {
      private$update_page_contents()
      private$update_graphics_state_dict()
      paste(self$version, self$body, self$xref, self$trailer, sep = "\n")
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Print
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print = function(...) {
      cat(self$as_character(...))
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Save to PDF file
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    save = function(filename, ...) {
      writeLines(self$as_character(), filename)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Deep copy
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    copy = function() {
      self$clone(deep = TRUE)
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
      gs_dicts <- vapply(gs, function(x) {glue("/GS{x[1]}{x[2]} <</ca {x[1]} /CA {x[2]}>>")}, character(1))
      gs_dict  <- paste(gs_dicts, collapse = " ")
      gs_dict  <- paste("<<", gs_dict, ">>")
      # Overwrite the ExtGSState to encompass all the graphics states we
      # get from the user objects
      self$setup_objects[[4]]$update(ExtGState = gs_dict)
    }
  ),


  active = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # List containing all objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    all_objects = function() {
      c(self$setup_objects, self$user_objects)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # All strings for all objects
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
    # The header for this document
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    header = function() {
      self$version
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The body is just the collection of all objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    body = function() {
      trimws(paste(self$objs, collapse = ""))
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Update a xref table based upon the current objects
    # and the offsets to the start of each object
    # Each xref entry must be exactly 20bytes (include CRLF).  Since I'm only
    # using CR, I need to add an extra space at the end of each xref entry
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xref = function() {
      all_offsets    <- self$offsets
      object_offsets <- head(all_offsets, -1L)
      xref <- c(
        "xref",
        glue("0 {length(self$all_objects) + 1L}"),
        "0000000000 65535 f ",
        sprintf("%010i 00000 n ", object_offsets)
      )

      paste(xref, collapse = "\n")
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # For the XREF table, need to know the offset to each object from start of document
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    offsets = function() {
      cumsum(nchar(c(self$header, self$objs)))
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The trailer indicates the start of the XREF table and total length
    # of all objects it represents (including the dummy/root object)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    trailer = function() {
      start_xref <- tail(self$offsets, 1) # byte index of start of xref table

      trailer <- c(
        glue::glue("trailer <</Size {length(self$all_objects) + 1}/Root 1 0 R>>"),
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
pdfdoc <- function(..., width = 400, height = 400, fontname = 'Helvetica', version = 1.2) {
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















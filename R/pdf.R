

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an empty shell for the PDF intermediate format
#' 
#' @return List with attributes. List items are PDF objects.  Attributes
#'         are PDF settings
#' @examples
#' create_pdf()
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pdf <- function() {
  doc <- list(
    page = list(
      objs  = list(), 
      gs    = list(pdf_dict(CA = 1, ca = 0)), # stroke + fill alpha
      fonts = list()
    )
  )
  # doc <- as.environment(doc)
  class(doc) <- 'pdf_doc'
  
  doc
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# If this 'gs' already exists on the doc, then return the index
# otherwise return NULL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gs_idx <- function(doc, gs) {

  stopifnot(!is.null(gs))
  stopifnot(inherits(doc, 'pdf_doc'))

  page <- doc$page

  for (i in seq_along(page$gs)) {
    if (identical(page$gs[[i]], gs)) {
      return(i)
    }
  }

  NULL
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Unconditionally add a 'gs' object
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_gs <- function(doc, gs) {
  page <- doc$page

  page$gs[[length(page$gs) + 1L]] <- gs

  doc$page <- page
  doc
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print a 'pdf' object
#' 
#' @param x pdf object
#' @param ... ignored
#' @return None
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.pdf_doc <- function(x, ...) {
  cat("<pdf doc> with", length(x), "pages\n")
  page <- x$page
  cat(sprintf("  Page 1: %i objects\n", length(page$objs)))
  invisible(x)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add a 'dict' or stream to the 'pdf'
#' 
#' @param doc pdf_doc
#' @param x pdf_dict or pdf_stream
#' @param pos position at which to add item. Item currently at this
#'        position will be moved to next position
#' @return pdf
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_add <- function(doc, x, pos = NULL) {
  stopifnot(is_dict(x) || is_stream(x))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For a stream object, keep track of the 'graphics state dict' for this
  # object. If it isn't already present on the 'resources' for this page
  # then add it.
  # Add the integer index (gs_ref) to the stream object so we 
  # add in the reference
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is_stream(x) && !is.null(x$gp)) {
    gs_dict <- gp_to_gs_dict(x$gp)
    if (is.null(gs_dict)) {
      x$gs_ref <- NULL
    } else {
      cur_idx <- gs_idx(doc, gs_dict)
      if (!is.null(cur_idx)) {
        x$gs_ref <- cur_idx
      } else {
        doc <- add_gs(doc, gs_dict)
        page     <- doc$page
        x$gs_ref <- length(page$gs)
      }
    }
  }
  
  
  if (is.null(pos)) {
    doc$page$objs <- append(doc$page$objs, list(x))
  } else {
    doc$page$objs <- append(doc$page$objs, list(x), after = pos - 1L)
  }
  
  doc
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' export pdf
#'
#' @param doc pdf_doc
#' @param filename defualt: NULL -> return string
#' @return string
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_render <- function(doc, filename = NULL) {
  
  fontname <- 'Helvetica'
  width    <- 400
  height   <- 400
  
  # Render "/Resources" object
  # Render "/Page" objects
  # Render "/Pages" object to point to "/Page" objects
  # Render "/Catalog" object to point to "/Pages" object
  
  idx_catalog    <- 1L
  idx_pages      <- idx_catalog + 1L
  
  idx_page1      <- idx_pages + 1L
  idx_resources1 <- idx_page1 + 1L
  
  len_resources1 <- 1L
  
  idx_fonts1 <- idx_resources1 + len_resources1
  len_fonts1 <- 1L
  
  idx_objs_start1 <- idx_fonts1 + len_fonts1
  n_objs_page1    <- length(doc$page$objs)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # /Catalog 
  #    - one/document
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  doc <- pdf_add(doc, pdf_dict(Type = '/Catalog', Pages = "2 0 R"), pos = 1)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # /Pages
  #   - one/document
  #   - Linked from /Catalog
  #   - Each page is an index list of objects
  #   - /Pages just points to the index lists for each page
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  doc <- pdf_add(doc, pdf_dict(Type = '/Pages'  , Kids  = "[3 0 R]", Count = 1), pos = 2)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # /Page
  #   - one/page
  #   - linked from /Pages
  #   - links to /Resources
  #   - contains a list of objects it points to
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  contents <- seq_len(n_objs_page1) + idx_objs_start1 - 1L 
  contents <- sprintf("%i 0 R", contents)
  contents <- paste(contents, collapse = " ")
  contents <- paste0("[", contents, "]")
  
  doc <- pdf_add(
    doc, 
    pdf_dict(
      Type      = '/Page',
      Parent    = "2 0 R",
      Resources = "4 0 R",
      MediaBox  = glue::glue("[0 0 {width} {height}]"),
      Contents  = contents
    ),
    pos = 3
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Resources
  #    - one/page
  #    - linked from /Page
  #    - links to font definitions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gs <- doc$page$gs
  names(gs) <- paste0("GS", seq_along(gs))
  gs <- do.call(pdf_dict, gs)
  
  doc <- pdf_add(
    doc, 
    pdf_dict(
      Font      = pdf_dict(F1 = "5 0 R"),
      # ExtGState = pdf_dict(GS11 = pdf_dict(ca = 1, CA = 1))
      ExtGState = gs
      
    ),
    pos = 4
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Font definition
  #    - N/page
  #    - Linked from /Resources
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  doc <- pdf_add(
    doc, 
    pdf_dict(
      Type     = '/Font', 
      Subtype  = "/Type1", 
      BaseFont = paste0("/", fontname)
    ),
    pos = 5
  )
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get sizes of all elements
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  s <- vapply(seq_along(doc$page$objs), function(i) {
    glue::glue(
      "{i} 0 obj
      {as.character(doc$page$objs[[i]])}
      endobj"
    )
  }, character(1))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Header
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  s <- c("%PDF-2.0", s)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # byte offsets (from start) for each obj.  
  # +1 for "\n" which will be inserted in a following step
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lens <- nchar(s) + 1L 
  
  offsets <- cumsum(lens)
  startxref <- offsets[length(offsets)]  # end of last object will be start of xref
  offsets   <- offsets[-length(offsets)] # but this index not part of xref table
  
  # Format xref offsets.  
  #   - byte offsets are 10 characters long. Padded with 0s
  #   - Every object is Version = 00000
  offsets <- sprintf("%010i 00000 n", offsets)
  offsets <- paste(offsets, collapse = "\n")
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # xref and trailer
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xref <- glue::glue(
    "xref
    0 {length(lens)}
    0000000000 65535 f
    {offsets}
    trailer <</Size {length(lens)}/Root 1 0 R>>
    startxref
    {startxref}
    %%EOF
    "
  )
  
  # append xref to total 
  s <- c(s, xref)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assemble full string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  s <- paste(s, collapse = "\n")
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return full string to user if not writing it to file
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(filename)) {
    s
  } else {
    writeLines(s, filename)
    invisible(s)
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Demo
#' @noRd
#' @importFrom grDevices colors
#' @importFrom stats runif
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tt <- function() {
  doc <- create_pdf()
  
  rr <- pdf_rect(120, 120, 200, 100, fill = sample(colors(), 1), alpha = 0.5)
  doc <- pdf_add(doc, rr)


  ll <- pdf_line(20, 0, 120, 200, col = 'blue', lwd = 20, lineend = 'butt', lty = 3)
  doc <- pdf_add(doc, ll)
  
  N  <- 100
  xs <- runif(N, 1, 400)
  ys <- runif(N, 1, 400)
  pl <- pdf_polyline(xs, ys, col = 'darkgreen')
  doc <- pdf_add(doc, pl)
  
  
  
  N  <- 3
  xs <- c(100, 300, 300)
  ys <- c(100, 100, 300)
  pl <- pdf_polygon(xs, ys, col = 'black', fill = "#ff000080")
  doc <- pdf_add(doc, pl)
  
  
  doc
  pdf_render(doc) |> cat()
  pdf_render(doc, "working/test.pdf")
  invisible(doc)
}



if (FALSE) {
  

}












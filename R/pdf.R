

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
      objs      = list(), 
      resources = list()
    )
  )
  class(doc) <- 'pdf_doc'
  
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
  doc <- pdf_add(
    doc, 
    pdf_dict(
      Font      = pdf_dict(F1 = "5 0 R"),
      ExtGState = pdf_dict(GS11 = pdf_dict(ca = 1, CA = 1))
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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tt <- function() {
  doc <- create_pdf()
  ll <- pdf_line(0, 0, 100, 100)
  doc <- pdf_add(doc, ll)
  
  rr <- pdf_rect(120, 120, 200, 100, fill = sample(colors(), 1), col = NA)
  doc <- pdf_add(doc, rr)


  ll <- pdf_line(20, 0, 120, 200, col = 'blue', lwd = 20, lineend = 'butt', lty = 3)
  doc <- pdf_add(doc, ll)

  ll <- pdf_line(220, 50, 400, 400, col = 'green', lty = 2)
  doc <- pdf_add(doc, ll)
  
  doc
  pdf_render(doc) |> cat()
  pdf_render(doc, "working/test.pdf")
  invisible(pdf_render(doc))
}



if (FALSE) {
  

}












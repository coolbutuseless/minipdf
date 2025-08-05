

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an empty shell for the PDF intermediate format
#' 
#' @param ... options
#' @return List with attributes. List items are PDF objects.  Attributes
#'         are PDF settings
#' @examples
#' create_pdf()
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pdf <- function(...) {
  doc <- list()
  atts <- list(...) 
  if (length(atts) > 0) {
    stopifnot(all_named(atts))
    attributes(doc) <- atts
  }
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
  cat("<pdf doc> with", length(x), "PDF objects\n")
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
    res <- append(doc, list(x))
  } else {
    res <- append(doc, list(x), after = pos - 1L)
  }
  
  structure(res, class = "pdf_doc")
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
  
  # Render "/Resources" object
  # Render "/Page" objects
  # Render "/Pages" object to point to "/Page" objects
  # Render "/Catalog" object to point to "/Pages" object
  
  fontname <- 'Helvetica'
  width <- 400
  height <- 400
  
  doc <- pdf_add(doc, pdf_dict(Type = '/Catalog', Pages = "2 0 R"), pos = 1)
  doc <- pdf_add(doc, pdf_dict(Type = '/Pages', Kids = "[3 0 R]", Count = 1), pos = 2)
  doc <- pdf_add(
    doc, 
    pdf_dict(
      Type      = '/Page',
      Parent    = "2 0 R",
      Resources = "4 0 R",
      MediaBox  = glue::glue("[0 0 {width} {height}]"),
      Contents  = "[6 0 R 7 0 R]"
    ),
    pos = 3
  )
  doc <- pdf_add(
    doc, 
    pdf_dict(
      Font      = pdf_dict(F1 = "5 0 R"),
      ExtGState = pdf_dict(GS11 = pdf_dict(ca = 1, CA = 1))
    ),
    pos = 4
  )
  doc <- pdf_add(
    doc, 
    pdf_dict(
      Type = '/Font', 
      Subtype = "/Type1", 
      BaseFont = paste0("/", fontname)
    ),
    pos = 5
  )
  
  s <- vapply(seq_along(doc), function(i) {
    glue::glue(
      "{i} 0 obj
      {as.character(doc[[i]])}
      endobj"
    )
  }, character(1))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate xref and trailer
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  s <- c("%PDF-2.0", s)
  lens <- nchar(s) + 1L
  
  offsets <- cumsum(lens)
  startxref <- offsets[length(offsets)]
  offsets <- offsets[-length(offsets)]
  
  offsets <- sprintf("%010i 00000 n", offsets)
  offsets <- paste(offsets, collapse = "\n")
  
  
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
  
  s <- c(s, xref)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assemble full string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  s <- paste(s, collapse = "\n")
  
  
  if (is.null(filename)) {
    s
  } else {
    writeLines(s, filename)
    invisible(s)
  }
  
  
}



if (FALSE) {
  
  doc <- create_pdf()
  ll <- pdf_line(0, 0, 100, 100)
  doc <- pdf_add(doc, ll)
  
  rr <- pdf_rect(120, 120, 200, 100)
  doc <- pdf_add(doc, rr)
  
  
  # ll <- pdf_line(20, 0, 120, 0)
  # doc <- pdf_add(doc, ll)
  
  doc
  pdf_render(doc) |> cat()
  pdf_render(doc, "working/test.pdf")

}












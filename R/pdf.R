

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
#' Create a PDF Dict object 
#' @param ... named arguments
#' @return 'pdf_dict' object
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_dict <- function(...) {
  dict <- list(...)
  if (!all_named(dict)) {
    print(dict)
    stop("pdf_dict(): Not all named")
  }
  class(dict) <- 'pdf_dict'
  dict
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert dict to character
#' 
#' @param x pdf_dict
#' @param depth print depth. Default: 0.  Used to control indentation
#' @param ... ignored
#' @return None.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.pdf_dict <- function(x, depth = 0, ...) {
  indent1 <- paste0(rep("  ", depth), collapse = "")
  indent2 <- paste0(rep("  ", depth + 1), collapse = "")
  nms   <- names(x)
  elems <- unname(x)
  
  s <- lapply(seq_along(nms), \(i) {
    if (is_dict(elems[[i]])) {
      glue::glue("{indent2}/{nms[i]}\n{as.character(elems[[i]], depth = depth + 1)}")
    } else {
      glue::glue("{indent2}/{nms[i]} {as.character(elems[[i]])}")
    }
  })
  s <- paste(s, collapse = "\n")
  glue::glue("{indent1}<<\n{s}\n{indent1}>>")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print a pdf Can be nested
#' @param x pdf_dict
#' @param depth print depth. Default: 0.  Used to control indentation
#' @param ... ignored
#' @return None.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.pdf_dict <- function(x, depth = 0, ...) {
  cat("<dict>\n")
  cat(as.character(x), "\n")
  invisible(x)
}


if (FALSE) {
  zz <- pdf_dict(type = "Hello", greg = pdf_dict(x = "next"))
  zz
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check an object is a dict
#' 
#' @param x obj
#' @return logical
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_dict <- function(x) {
  isTRUE(inherits(x, 'pdf_dict'))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Assert an object is a dict
#' 
#' @param x obj
#' @return None
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
assert_dict <- function(x) {
  stopifnot(is_dict(x))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add a 'dict' or stream to the 'pdf'
#' 
#' @param doc pdf_doc
#' @param x pdf_dict or pdf_stream
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
#' Create a pdf stream object
#' @param ... named arguments
#' @return 'stream' object
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_stream <- function(type, ...) {
  stream <- list(...)
  if (!all_named(stream)) {
    print(stream)
    stop("Not all named")
  }
  class(stream) <- 'pdf_stream'
  attr(stream, 'type') <- type
  stream
}

is_stream <- function(x) {
  isTRUE(inherits(x, 'pdf_stream'))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a line in pdf
#' @param x1,y1,x2,y2 endpoints
#' @return stream object
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_line <- function(x1, y1, x2, y2) {
  pdf_stream(type = 'line', x1 = x1, y1 = y1, x2 = x2, y2 = y2)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert stream to character
#' 
#' @param x pdf_stream object
#' @param ... ignored
#' @return character string
#' @importFrom glue glue glue_data
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.character.pdf_stream <- function(x, ...) {
  type <- attr(x, 'type', exact = TRUE)
  if (type == 'line') {
    gp <- paste(c(
      "5 w", 
      "0 0 0 rg", 
      "1 0 0 RG", 
      ""
    ), collapse = "\n")
    s <- glue::glue_data(x, "{gp}{x1} {y1} m {x2} {y2} l s")
  } else {
    stop("Unknown stream: ", deparse1(class(x)))
  }
  
  s <- paste('q', s, 'Q', sep = "\n")
  len <- nchar(s)
  len <- as.character(pdf_dict(Length = len))
  s <- paste(len, "stream", s, "endstream", sep = "\n")
  s
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' print stream
#' @param x pdf_stream
#' @param ... ignored
#' @return None
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.pdf_stream <- function(x, ...) {
  cat("<stream: ")
  type <- attr(x, 'type', exact = TRUE)
  cat(type, ">\n", sep = "")
  cat(as.character(x, ...), "\n", sep = "")
  invisible(x)
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
  
  doc <- pdf_add(doc, pdf_dict(Type = '/Catalog', Pages = "2 0 R"), pos = 1)
  doc <- pdf_add(doc, pdf_dict(Type = '/Pages', Kids = "[3 0 R]", Count = 1), pos = 2)
  doc <- pdf_add(
    doc, 
    pdf_dict(
      Type      = '/Page',
      Parent    = "2 0 R",
      Resources = "4 0 R",
      MediaBox  = glue::glue("[0 0 {width} {height}]"),
      Contents  = "[6 0 R]"
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
  fontname <- 'Helvetica'
  width <- 400
  height <- 300
  
  doc <- create_pdf()
  ll <- pdf_line(0, 0, 100, 100)
  doc <- pdf_add(doc, ll)
  doc
  pdf_render(doc) |> cat()

}












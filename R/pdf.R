

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
  
  im <- matrix(seq(0, 255), nrow = 8, ncol = 32)
  
  doc <- list(
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Initialise with an empty first page
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    page = list(
      list()  # Page 1
    ), 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Initial graphics state is color and fill both have alpha = 1
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    gs = list(
      pdf_dict(CA = 1, ca = 1)
    ),
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Images
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    image = list(im)
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

  for (i in seq_along(doc$gs)) {
    if (identical(doc$gs[[i]], gs)) {
      return(i)
    }
  }

  NULL
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Unconditionally add a 'gs' object
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
add_gs <- function(doc, gs) {
  doc$gs[[length(doc$gs) + 1L]] <- gs
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
  cat(sprintf("  Page 1: %i objects\n", length(page[[1]])))
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
  stopifnot(is_dict(x) || is_stream(x) || is.character(x))
  stopifnot(inherits(doc, 'pdf_doc'))
  
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
        x$gs_ref <- length(doc$gs)
      }
    }
  }
  
  
  if (is.null(pos)) {
    doc$page[[1]] <- append(doc$page[[1]], list(x))
  } else {
    doc$page[[1]] <- append(doc$page[[1]], list(x), after = pos - 1L)
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
  
  width    <- 400
  height   <- 400
  
  
  idx_catalog  <- 1L
  len_catalog  <- 1L
  
  idx_pages    <- idx_catalog + len_catalog
  len_pages    <- 1L
  
  idx_resources <- idx_pages + len_pages
  len_resources <- 1L
  
  idx_xobjects <- idx_resources + len_resources
  len_xobjects <- length(doc$image) * 2  # 1 for the image, 1 for the alpha mask
  
  idx_page1      <- idx_xobjects + len_xobjects
  len_page1      <- 1L
  
  idx_page1_objs <- idx_page1 + len_page1
  len_page1_objs <- length(doc$page[[1]])
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # /Catalog 
  #    - one/document
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  doc <- pdf_add(doc, pdf_dict(
    Type = '/Catalog', 
    Pages = glue::glue("{idx_pages} 0 R")
  ), pos = idx_catalog)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # /Pages
  #   - one/document
  #   - Linked from /Catalog
  #   - Each page is an index list of objects
  #   - /Pages just points to the index lists for each page
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  doc <- pdf_add(doc, pdf_dict(
    Type      = '/Pages'  , 
    Resources = glue::glue("{idx_resources} 0 R"),
    Kids      = glue::glue("[{idx_page1} 0 R]"), 
    Count = 1
  ), pos = idx_pages)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Resources
  #    - one/doc
  #    - linked from /Pages
  #    - Defines the standard fonts
  #    - Defines the graphics states
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gs <- doc$gs
  names(gs) <- paste0("GS", seq_along(gs))
  gs <- do.call(pdf_dict, gs)
  
  xobj <- pdf_dict(Im1 = glue::glue("{idx_xobjects} 0 R"))
  
  
  doc <- pdf_add(
    doc, 
    pdf_dict(
      XObject = xobj,
      ExtGState = gs,
      Font = pdf_dict(
        F1  = pdf_dict(Type='/Font',  Subtype ="/Type1",  BaseFont='/Helvetica'            ),
        F2  = pdf_dict(Type='/Font',  Subtype ="/Type1",  BaseFont='/Helvetica-Bold'       ),
        F3  = pdf_dict(Type='/Font',  Subtype ="/Type1",  BaseFont='/Helvetica-Oblique'    ),
        F4  = pdf_dict(Type='/Font',  Subtype ="/Type1",  BaseFont='/Helvetica-BoldOblique'),
        F5  = pdf_dict(Type='/Font',  Subtype ="/Type1",  BaseFont='/Courier'              ),
        F6  = pdf_dict(Type='/Font',  Subtype ="/Type1",  BaseFont='/Courier-Bold'         ),
        F7  = pdf_dict(Type='/Font',  Subtype ="/Type1",  BaseFont='/Courier-Oblique'      ),
        F8  = pdf_dict(Type='/Font',  Subtype ="/Type1",  BaseFont='/Courier-BoldOblique'  ),
        F9  = pdf_dict(Type='/Font',  Subtype ="/Type1",  BaseFont='/Times-Roman'          ),
        F10 = pdf_dict(Type='/Font',  Subtype ="/Type1",  BaseFont='/Times-Bold'           ),
        F11 = pdf_dict(Type='/Font',  Subtype ="/Type1",  BaseFont='/Times-Italic'         ),
        F12 = pdf_dict(Type='/Font',  Subtype ="/Type1",  BaseFont='/Times-BoldItalic'     ),
        F13 = pdf_dict(Type='/Font',  Subtype ="/Type1",  BaseFont='/Symbol'               ),
        F14 = pdf_dict(Type='/Font',  Subtype ="/Type1",  BaseFont='/ZapfDingbats'         )
      )
    ),
    pos = idx_resources
  )
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Manually create an image object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  im <- doc$image[[1]]
  
  im_bytes <- 
    im |>
    t() |>
    as.raw() |> 
    as.character() |> 
    paste0(collapse = "") 
  
  w <- ncol(im)
  h <- nrow(im)
  
  im_dict <- pdf_dict(
    Type             = "/XObject",
    Subtype          = "/Image",
    Width            = w,
    Height           = h,
    ColorSpace       = "/DeviceGray",
    BitsPerComponent = 8,
    Length           = w * h * 2,
    Filter           = "/ASCIIHexDecode",
    SMask            = glue::glue("{idx_xobjects + 1} 0 R")
  )
  
  s <- paste(
    as.character(im_dict),
    "stream",
    im_bytes,
    "endstream",
    sep = "\n"
  )
  
  doc <- pdf_add(
    doc, 
    s,
    pos = idx_xobjects
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  im <- 255 - doc$image[[1]]
  alpha_bytes <- 
    im |>
    t() |>
    as.raw() |> 
    as.character() |> 
    paste0(collapse = "")
  
  w <- ncol(im)
  h <- nrow(im)
  
  alpha_dict <- pdf_dict(
    Type             = "/XObject",
    Subtype          = "/Image",
    Width            = w,
    Height           = h,
    ColorSpace       = "/DeviceGray",
    BitsPerComponent = 8,
    Length           = w * h * 2,
    Filter           = "/ASCIIHexDecode"
  )
  
  alpha_mask <- paste(
    as.character(alpha_dict),
    "stream",
    alpha_bytes,
    "endstream",
    sep = "\n"
  )
  
  doc <- pdf_add(
    doc, 
    alpha_mask,
    pos = idx_xobjects + 1L
  )
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # /Page
  #   - one/page
  #   - linked from /Pages
  #   - links to /Resources
  #   - contains a list of objects it points to
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  contents <- seq_len(len_page1_objs) + idx_page1_objs - 1L 
  contents <- sprintf("%i 0 R", contents)
  contents <- paste(contents, collapse = " ")
  contents <- paste0("[", contents, "]")
  
  doc <- pdf_add(
    doc, 
    pdf_dict(
      Type      = '/Page',
      Parent    = glue::glue("{idx_pages} 0 R"),
      MediaBox  = glue::glue("[0 0 {width} {height}]"),
      Contents  = contents
    ),
    pos = idx_page1
  )
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get sizes of all elements
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  page_objs <- doc$page[[1]]
  s <- vapply(seq_along(page_objs), function(i) {
    glue::glue(
      "{i} 0 obj
      {as.character(page_objs[[i]])}
      endobj"
    )
  }, character(1))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Header
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  s <- c("%PDF-1.7", s)
  
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
  
  doc <- pdf_rect(doc, 120, 120, 200, 100, fill = sample(colors(), 1), alpha = 0.8)
  doc <- pdf_line(doc, 20, 0, 120, 200, col = 'blue', lwd = 20, lineend = 'butt', lty = 3)

  N  <- 10
  xs <- runif(N, 1, 400)
  ys <- runif(N, 1, 400)
  doc <- pdf_polyline(doc, xs, ys, col = 'darkgreen', alpha = 0.2)

  xs <- c(100, 300, 300)
  ys <- c(100, 100, 300)
  doc <- pdf_polygon(doc, xs, ys, col = 'black', fill = "#ff000080")

  doc <- pdf_circle(doc, 300, 300, 100, col = 'hotpink', fill = '#00ff0080')

  doc <- pdf_text(doc, "Hello #RStats", 50, 50, fontsize = 40, fill = 'black', col = 'hotpink',
                  fontfamily = "mono", fontface = 'bold.italic', mode = 2)


  w <- 10
  h <- 10
  im <- matrix(sample(256L, w * h) - 1L, w, h)
  doc <- pdf_image(doc, im, x = 50, y = 50, scale = 1)
  
  
  doc
  pdf_render(doc) |> cat()
  pdf_render(doc, "working/test.pdf")
  invisible(doc)
}



if (FALSE) {
  
  set.seed(1)
  doc <- create_pdf()
  
  N <- 100
  xs <- sample(400, N, TRUE)
  ys <- sample(400, N, TRUE)
  rs <- sample(100, N, TRUE)
  cs <- sample(colors(), N, TRUE)

  for (i in seq_len(N)) {
    doc <- pdf_circle(doc, xs[i], ys[i], rs[i], col = NA, fill = cs[i], alpha = 0.2)
  }

  cs <- rainbow(400)
  for (i in seq(1, 400, 10)) {
    doc <- pdf_line(doc, i, 0, 0, 400 - i, col = cs[i], alpha = 0.2)
  }
  
  doc <- pdf_text(doc, "Hello", 20, 300, fontsize = 90, mode = 0, fill = 'black', 
                 fontface = 'plain')

  doc <- pdf_text(doc, "#RStats", 20, 200, fontsize = 90, mode = 1, col = 'hotpink', 
                 fontface = 'bold.italic', lwd = 5)

  pdf_render(doc, "working/test.pdf")
  

}












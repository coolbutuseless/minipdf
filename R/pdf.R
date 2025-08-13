

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sanitise and prepare a string for pdf inclustion
#' @param x string
#' @return escpaed string ready for inclusion for PDF
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_pdf_text <- function(x) {
  
  if (is.null(x) || length(x) == 0) {
    NULL
  } else {
    stopifnot(length(x) == 1)
    paste0("(", x, ")")
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Start a new page
#' 
#' @param doc pdf_doc
#' @return doc with new page added (and made the current page)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf_newpage <- function(doc) {
  
  doc$page_num <- doc$page_num + 1;
  doc$page <- append(doc$page, list(list()))
  
  # Do this so every page has at least one object in it
  doc <- pdf_clip_rect(doc, 0, 0, doc$width, doc$height)
  
  doc
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an empty shell for the PDF intermediate format
#' 
#' @param width,height page size
#' @param title title
#' @param author author
#' @param creator creator
#' @param creation_date creation date
#' @return List with attributes. List items are PDF objects.  Attributes
#'         are PDF settings
#' @examples
#' create_pdf()
#' 
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pdf <- function(width = 400, height = 400, 
                       title = NULL, author = NULL, creator = "minipdf/R", 
                       creation_date = strftime(Sys.time(), format = "D:%Y%m%d%H%M")) {
  
  doc <- list(
    
    width  = width,
    height = height,
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Initialise with an empty first page
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    page_num = 0L,
    page = list(
      # empty 
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
    # image = list(matrix(seq(0, 255), nrow = 16, ncol = 16))
    image = list()
  )
  # doc <- as.environment(doc)
  class(doc) <- 'pdf_doc'
  
  
  doc <- pdf_newpage(doc)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add Document Level meta-info
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  doc$info <- pdf_dict(
    Title        = as_pdf_text(title),
    Author       = as_pdf_text(author),
    Creator      = as_pdf_text(creator),
    CreationDate = as_pdf_text(creation_date)
  )
  
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
    doc$page[[doc$page_num]] <- append(
      doc$page[[doc$page_num]], 
      list(x)
    )
  } else {
    doc$page[[doc$page_num]] <- append(
      doc$page[[doc$page_num]], 
      list(x), 
      after = pos - 1L
    )
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
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure that no scientific notion is used in the PDF output
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  opts <- options(scipen = 9999)
  on.exit(options(opts))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate indices for objects
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  doc$page_num <- 1L # Add all meta-objects (catalog, pages, resources etc to first page)
  
  idx_docinfo <- 1L
  len_docinfo <- 0L
  
  idx_catalog  <- idx_docinfo + len_docinfo
  len_catalog  <- 1L
  
  idx_pages    <- idx_catalog + len_catalog
  len_pages    <- 1L
  
  idx_resources <- idx_pages + len_pages
  len_resources <- 1L
  
  idx_xobjects <- idx_resources + len_resources
  len_xobjects <- length(doc$image) * 2  # 1 for the image, 1 for the alpha mask
  
  idx_page_start <- idx_xobjects + len_xobjects
  idx_page_len   <- length(doc$page)

  idx_objs  <- idx_page_start + idx_page_len
  lens_objs <- lengths(doc$page)

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
  
  kids <- seq_len(length(doc$page)) + (idx_page_start - 1L)
  kids <- paste(kids, "0 R", collapse = " ")
  kids <- paste("[", kids, "]")
  
  doc <- pdf_add(doc, pdf_dict(
    Type      = '/Pages'  , 
    Resources = glue::glue("{idx_resources} 0 R"),
    MediaBox  = glue::glue_data(doc, "[0 0 {width} {height}]"),
    Kids      = kids, 
    Count = length(doc$page)
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
  
  
  if (length(doc$image) == 0) {
    xobj = NULL
  } else {
    im_nms  <- paste0("Im", seq_along(doc$image))
    im_idxs <- idx_xobjects + (seq_along(doc$image) - 1L) * 2
    im_refs <- glue::glue("{im_idxs} 0 R") |> as.list()
    names(im_refs) <- im_nms
  
    xobj <- do.call(pdf_dict, im_refs)
  }
  
  
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
  obj_idx <- idx_xobjects
  
  for (i in seq_along(doc$image)) {
    im_raw <- doc$image[[i]]
    im <- image_to_bytes(im_raw)
    
    # filter <- "/ASCIIHexDecode"
    # im_bytes <- im$pixels |> enc_hex()
    
    filter <- "/ASCII85Decode"
    im_bytes <- im$pixels |> enc_ascii85()

    im_dict <- pdf_dict(
      Type             = "/XObject",
      Subtype          = "/Image",
      Width            = im$width,
      Height           = im$height,
      ColorSpace       = im$colorspace,
      BitsPerComponent = 8,
      Length           = nchar(im_bytes),
      Filter           = filter,
      Interpolate      = ifelse(isTRUE(attr(im_raw, 'interpolate')), 'true', 'false'),
      SMask            = glue::glue("{obj_idx + 1} 0 R") # Refer to the soft mask
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
      pos = obj_idx
    )
    
    obj_idx <- obj_idx + 1L
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Write the alpha image as a soft mask
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # filter      <- "/ASCIIHexDecode"
    # alpha_bytes <- im$alpha |> enc_hex()
    # 
    # filter      <- "/ASCII85Decode"
    # alpha_bytes <- im$alpha |> enc_ascii85()

    filter      <- "[/ASCII85Decode /RunLengthDecode]"
    alpha_bytes <- im$alpha |>
      enc_rle() |>
      enc_ascii85()
    
    alpha_dict <- pdf_dict(
      Type             = "/XObject",
      Subtype          = "/Image",
      Width            = im$width,
      Height           = im$height,
      ColorSpace       = "/DeviceGray", # Alpha channel is always just 'gray'
      BitsPerComponent = 8,
      Length           = nchar(alpha_bytes),
      Filter           = filter,
      Interpolate      = ifelse(isTRUE(attr(im_raw, 'interpolate')), 'true', 'false')
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
      pos = obj_idx
    )
    
    obj_idx <- obj_idx + 1L
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # /Page
  #   - one/page
  #   - linked from /Pages
  #   - links to /Resources
  #   - contains a list of objects it points to
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For page, add a /page object with contents
  
  page_num <- 1L
  nobjs_prior <- 0L
  for (page_num in seq_along(doc$page)) {
  # {
    # Assemble a list of references to all pages
    contents <- seq_len(lens_objs[page_num]) + (idx_objs - 1L) + nobjs_prior
    contents <- sprintf("%i 0 R", contents)
    contents <- paste(contents, collapse = " ")
    contents <- paste0("[", contents, "]")
    
    # message(">>> Page ", page_num, " = ", (idx_page_start - 1L) + page_num)
    
    doc <- pdf_add(
      doc, 
      pdf_dict(
        Type      = '/Page',
        Parent    = glue::glue("{idx_pages} 0 R"),
        Contents  = contents
      ),
      pos = (idx_page_start - 1L) + page_num
    )
    
    nobjs_prior <- nobjs_prior + lens_objs[page_num]
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get sizes of all elements
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  page_objs <- do.call(c, doc$page)
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
    trailer <</Size {length(lens)} /Root 1 0 R>>
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
#' @importFrom grDevices rainbow
#' @importFrom png readPNG
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tt <- function() {
  doc <- create_pdf(width = 600)
  
  doc <- pdf_rect(doc, 120, 120, 200, 100, fill = sample(colors(), 1), alpha = 0.8)
  doc <- pdf_line(doc, 20, 0, 120, 200, col = 'blue', lwd = 20, lineend = 'butt', lty = 3)
  
  doc <- pdf_clip_rect(doc, 80, 80, 300, 300); # Global clipping
  
  N  <- 10
  xs <- runif(N, 1, 400)
  ys <- runif(N, 1, 400)
  doc <- pdf_polyline(doc, xs, ys, col = 'darkgreen', alpha = 0.2)
  
  xs <- c(100, 300, 300)
  ys <- c(100, 100, 300)
  doc <- pdf_polygon(doc, xs, ys, col = 'black', fill = "#ff000080")
  
  doc <- pdf_circle(doc, 300, 300, 100, col = 'hotpink', fill = '#00ff0080')
  
  # doc <- pdf_clip_polygon(doc, xs = c(0, 0, 400), ys = c(0, 400, 0))
  
  
  doc <- pdf_text(doc, "Hello #RStats", 50, 50, fontsize = 40, fill = 'black', col = 'hotpink',
                  fontfamily = "mono", fontface = 'bold.italic', mode = 2,
                  tf = tf_rotate(0.5, 50, 50), 
                  clip = clip_polygon(xs = c(0, 0, 400), ys = c(0, 400, 0)))
  
  
  xs <- c(100, 300, 300, 100,   150, 250, 250, 150) + 50
  ys <- c(100, 100, 300, 300,   150, 150, 250, 250) + 50
  id <- c(1, 1, 1, 1, 2, 2, 2, 2)
  doc <- pdf_polygon(doc, xs, ys, id = id, rule = 'evenodd')
  
  
  w <- 10
  h <- 10
  im <- matrix(as.integer(100 + 50 * sin(8 * seq(w * h))), w, h)
  doc <- pdf_image(doc, im, x = 50, y = 50, scale = 10, interpolate = FALSE)
  
  
  w <- 10
  h <- 10
  im <- array(as.integer(0), dim = c(w, h, 2))
  im[,,1] <- 255
  im[,,2] <- 200
  
  doc <- pdf_image(doc, im, x = 150, y = 150, scale = 10)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  doc <- pdf_newpage(doc)
  
  theta <- seq(0, 359, 60) + 30
  xs1 <- 200 * cos(theta * pi/180) + 300
  ys1 <- 200 * sin(theta * pi/180) + 200
  xs2 <-  40 * cos(theta * pi/180) + 300
  ys2 <-  40 * sin(theta * pi/180) + 200

  xs <- c(xs1, xs2)
  ys <- c(ys1, ys2)
  id <- c(
    rep(1, length(xs1)),
    rep(2, length(xs2))
  )
  
  doc <- pdf_clip_polygon(doc, xs, ys, id = id, rule = 'evenodd')
  # doc <- pdf_clip_polygon(doc, xs1, ys1)
    
  
  N <- 100
  xs <- sample(600, N, TRUE)
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
  
  doc <- pdf_translate(doc, 120, -80)
  
  doc <- pdf_text(doc, "Hello", 20, 300, fontsize = 90, mode = 0, fill = 'black', 
                  fontface = 'plain')
  
  doc <- pdf_text(doc, "#RStats", 20, 200, fontsize = 80, mode = 1, col = 'royalblue', 
                  fontface = 'bold.italic', lwd = 5)
  
  
  
  im  <- png::readPNG(system.file("img", "Rlogo.png", package="png")) * 255
  doc <- pdf_image(doc, im, x = 250, y = 290, scale = 1)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  doc
  pdf_render(doc) |> cat()
  pdf_render(doc, "working/test.pdf")
  invisible(doc)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Demo
#' @noRd
#' @importFrom grDevices colors
#' @importFrom stats runif
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ttt <- function() {
  doc <- create_pdf(width = 600, height = 400)
  
  theta <- seq(0, 359, 60) + 30
  xs1 <- 200 * cos(theta * pi/180) + 300
  ys1 <- 200 * sin(theta * pi/180) + 200
  xs2 <-  80 * cos(theta * pi/180) + 300
  ys2 <-  80 * sin(theta * pi/180) + 200
  
  xs <- c(xs1, xs2)
  ys <- c(ys1, ys2)
  id <- c(
    rep(1, length(xs1)),
    rep(2, length(xs2))
  )
  
  doc <- pdf_clip_polygon(doc, xs, ys, id = id, rule = 'evenodd')
  # doc <- pdf_clip_polygon(doc, xs1, ys1)
  
  doc <- pdf_rect(doc, 20, 20, 360, 360)
  
  doc
  pdf_render(doc) |> cat()
  pdf_render(doc, "working/test.pdf")
  invisible(doc)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Demo
#' @noRd
#' @importFrom grDevices colors
#' @importFrom stats runif
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ee <- function() {
  doc <- create_pdf()
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


if (FALSE) {
  
  
  
}










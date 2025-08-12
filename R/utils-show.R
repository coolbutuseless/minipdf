

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Show the PDF using the defined viewer
#' 
#' @param doc pdf doc
#' @param viewer use the defined viewer. Default: utils::browseURL
#' @return None
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
show_pdf <- function(doc, viewer = getOption("viewer", utils::browseURL)) {
  temp_dir <- tempfile("viewpdf")
  dir.create(temp_dir)
  temp_pdf <- file.path(temp_dir, "temp.pdf")
  
  pdf_render(doc, filename = temp_pdf)
  
  if (!is.null(viewer)) {
    viewer(temp_pdf)
  } else {
    warning("No viewer available.")
  }
  invisible(temp_pdf)
}


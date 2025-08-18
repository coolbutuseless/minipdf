

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert image to bytes for RGB + Alpha (or Gray + Alpha)
#' 
#' @param im image. numeric values in range [0, 255]
#' \itemize{
#'   \item{Grayscale - numeric matrix}
#'   \item{Grayscale with alpha - numeric array with 2 planes}
#'   \item{RGB - numeric array with 3 planes}
#'   \item{RGB with alpha - numeric array with 4 planes}
#'   \item{RGB with alpha - nativeraster}
#' }
#' @return named list of 3 elements: type, rgb (or g) and alpha
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
image_to_bytes <- function(im) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare the return list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res           <- list()
  res$width     <- ncol(im)
  res$height    <- nrow(im)
  res$realalpha <- FALSE    # Is this a real alpha channel?
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Wrangle the pixels to the right order for inclusion in a PDF
  # Create a fake alpha channel if image does not have an alpha channel
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.matrix(im)) {
    res$colorspace <- '/DeviceGray' 
    res$pixels     <- t(im)
    res$alpha      <- rep(255, res$width * res$height) # Fake alpha
    res$realalpha  <- FALSE
  } else if (is.array(im)) {
    stopifnot(length(dim(im)) == 3)
    nplanes <- dim(im)[3]
    
    if (nplanes == 1) {
      res$colorspace <- '/DeviceGray' 
      res$pixels     <- t(im)
      res$alpha      <- rep(255, res$width * res$height) # Fake alpha
      res$realalpha  <- FALSE
    } else if (nplanes == 2) {
      res$colorspace <-  '/DeviceGray'
      res$pixels <- im[,,1] |> t()
      res$alpha  <- im[,,2] |> t()
      res$realalpha <- TRUE
    } else if (nplanes == 3) {
      res$pixels     <- aperm(im, c(3, 2, 1)) |> as.vector()
      res$colorspace <- '/DeviceRGB'
      res$alpha      <- rep(255, res$width * res$height) # Fake alpha
      res$realalpha  <- FALSE
    } else if (nplanes == 4) {
      res$colorspace <- '/DeviceRGB'
      alpha          <- im[, , 4]
      im             <- im[, , 1:3]
      res$pixels     <- aperm(im, c(3, 2, 1)) |> as.vector()
      res$colorspace <- '/DeviceRGB'
      res$alpha      <- t(alpha)
      res$realalpha  <- TRUE
    } else {
      stop("Unhandled num of planes: ", nplanes)
    }
  }
  
  res
}



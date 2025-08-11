

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
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
image_to_bytes <- function(im) {

  res        <- list()
  res$width  <- ncol(im)
  res$height <- nrow(im)
  
  if (is.matrix(im)) {
    res$colorspace <- '/DeviceGray' 
    res$pixels     <- t(im)
    res$alpha      <- rep(255, res$width * res$height) # Fake alpha
  } else if (is.array(im)) {
    stopifnot(length(dim(im)) == 3)
    nplanes <- dim(im)[3]
    
    if (nplanes == 1) {
      res$colorspace <- '/DeviceGray' 
      res$pixels     <- t(im)
      res$alpha      <- rep(255, res$width * res$height) # Fake alpha
    } else if (nplanes == 2) {
      res$colorspace <-  '/DeviceGray'
      res$pixels <- im[,,1] |> t()
      res$alpha  <- im[,,2] |> t()
    } else if (nplanes == 3) {
      res$pixels     <- aperm(im, c(3, 2, 1)) |> as.vector()
      res$colorspace <- '/DeviceRGB'
      res$alpha      <- rep(255, res$width * res$height) # Fake alpha
    } else if (nplanes == 4) {
      res$colorspace <- '/DeviceRGB'
      alpha          <- im[, , 4]
      im             <- im[, , 1:3]
      res$pixels     <- aperm(im, c(3, 2, 1)) |> as.vector()
      res$colorspace <- '/DeviceRGB'
      res$alpha       <- t(alpha)
    } else {
      stop("Unhandled num of planes: ", nplanes)
    }
    
  } else if (inherits(im, 'nativeRaster')) {
    res <- nr_to_bytes(im)
  }
  
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' NativeRaster to bytes
#' 
#' @param nr nativeRaster
#' @return named list of 3 elements: type, rgb (or g) and alpha
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nr_to_bytes <- function(nr) {
  stop("nr_to_bytes(): not done yet")
  
  res <- list()
  res$colorspace   <- '/DeviceRGB'
  res$width  <- ncol(nr)
  res$height <- nrow(nr)
  
  res$pixels <- NULL
  res$alpha  <- NULL
  
  res
}



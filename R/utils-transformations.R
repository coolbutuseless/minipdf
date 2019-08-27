

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Helper to create transformation strings
#'
#' For details on PDF's coordinate transformations, see the standard
#' section 8.3.4 Transformation Matrices
#'
#'
#' \itemize{
#' \item{\code{translate(x, y)} - translate an object}
#' \item{\code{scale(sx, sy=sx)} - scale an object. If only 1 number is given, the object is scaled the same in both directions}
#' \item{\code{rotate(angle, x=0, y=0)} - rotate an object around the given point by the given angle (measured in degrees). By default rotate about the origin}
#' \item{\code{skew(A, B)} - skew an object with skew angles A and B (in degrees)}
#' \item{\code{custom(a, b, c, d, e, f)} - specify the 6 elements of a custom transformation matrix}
#' }
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tf <- list(
  translate = function(x, y) {
    glue("1 0 0 1 {x} {y}")
  },

  scale = function(sx, sy = sx) {
    glue("{sx} 0 0 {sy} 0 0")
  },

  rotate = function(angle, x = NULL, y = NULL) {
    cosQ <- round(cos(angle * pi/180), 3)
    sinQ <- round(sin(angle * pi/180), 3)
    res <- glue("{cosQ} {sinQ} {-sinQ} {cosQ} 0 0")

    if (x != 0 || y != 0) {
      translate_origin1 <- tf$translate( x,  y) # glue("1 0 0 1 { x} { y} cm")
      translate_origin2 <- tf$translate(-x, -y) # glue("1 0 0 1 {-x} {-y} cm")
      res               <- c(translate_origin1, res, translate_origin2)
    }

    res

  },

  skew = function(A, B) {
    tanA <- round(tan(A * pi/180), 3)
    tanB <- round(tan(B * pi/180), 3)
    glue("1 {tanA} {tabB} 1 0 0")
  },

  custom = function(a, b, c, d, e, f) {
    glue("{a} {b} {c} {d} {e} {f}")
  }
)





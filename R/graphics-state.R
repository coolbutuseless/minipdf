


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create graphical parameters 
#' 
#' @param col,fill set graphics parameters for this object
#' @param alpha,rule options
#' @return a graphics parameter object
#' @examples
#' pgpar()
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pgpar <- function(
    col = 'black', 
    fill = NA,
    alpha = 1,
    rule = 'winding'  # or even odd
) {
  
  list(
    col   = col,
    fill  = fill,
    alpha = alpha,
    rule  = rule
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_transparent <- function(color) {
  grDevices::col2rgb(color, alpha = TRUE)[4] == 0
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert graphics parameters to graphics state operators
#' 
#' These are values which can be expressed inline with the object.
#' Some options (such as 'CA' and 'ca') can only be defined as part
#' of a graphics state parameter dictionary
#' 
#' @param gp named list 'gp' object as created by \code{\link{pgpar}()}
#' @return single string represeting graphics state
#' @noRd
#' @importFrom grDevices col2rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gp_to_gs_operators <- function(gp) {
  
  
  col   <- (grDevices::col2rgb(gp$col  , alpha = TRUE)/255) |> as.vector()
  fill  <- (grDevices::col2rgb(gp$fill , alpha = TRUE)/255) |> as.vector()
  
  col_alpha  <- glue::glue("{col[4] * gp$alpha} CA")
  fill_alpha <- glue::glue("{fill[4] * gp$alpha} ca")
  
  col  <- glue::glue("{col[1]} {col[2]} {col[3]} RG")
  fill <- glue::glue("{fill[1]} {fill[2]} {fill[3]} rg")
  
  
  glue::glue(
    "{col}
    {fill}"
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert graphics parameters to graphics state operators
#' 
#' These are values which can be expressed inline with the object.
#' Some options (such as 'CA' and 'ca') can only be defined as part
#' of a graphics state parameter dictionary
#' 
#' @param gp named list 'gp' object as created by \code{\link{pgpar}()}
#' @return single string represeting graphics state
#' @noRd
#' @importFrom grDevices col2rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gp_to_gs_dict <- function(gp) {
  
  col   <- (grDevices::col2rgb(gp$col  , alpha = TRUE)/255) |> as.vector()
  fill  <- (grDevices::col2rgb(gp$fill , alpha = TRUE)/255) |> as.vector()
  
  pdf_dict(
    CA = col[4]  * gp$alpha, # stroke alpha 
    ca = fill[4] * gp$alpha  # fill alpha
  )
}











#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create graphical parameters 
#' 
#' @param col,fill set graphics parameters for this object
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
  col2rgb(color, alpha = TRUE)[4] == 0
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gp_to_gs <- function(gp) {
  
  
  col   <- (col2rgb(gp$col  , alpha = TRUE)/255) |> as.vector()
  fill  <- (col2rgb(gp$fill , alpha = TRUE)/255) |> as.vector()
  
  col_alpha  <- glue::glue("{col[4] * gp$alpha} CA")
  fill_alpha <- glue::glue("{fill[4] * gp$alpha} ca")
  
  col  <- glue::glue("{col[1]} {col[2]} {col[3]} RG")
  fill <- glue::glue("{fill[1]} {fill[2]} {fill[3]} rg")
  
  
  glue::glue(
    "{col}
    {fill}
    {col_alpha}
    {fill_alpha}"
  )
}


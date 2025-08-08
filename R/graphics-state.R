


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create graphical parameters 
#' 
#' @param col,fill set graphics parameters for this object
#' @param alpha additional alpha applied to col, fill
#' @param lty,lwd,lineend,linejoin,linemitre line optins
#' @param fontsize,fontfamily,fontface font definition
#' @param rule fill rule. 'winding' (default) or 'evenodd'
#' @return a graphics parameter object
#' @examples
#' pgpar()
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pgpar <- function(
    col   = 'black', 
    fill  = 'black',
    alpha = 1,
    lty,
    lwd,
    lineend,
    linejoin,
    linemitre,
    fontsize,
    fontfamily,
    fontface,
    rule
) {
  
  find_args()
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_transparent <- function(color) {
  grDevices::col2rgb(color, alpha = TRUE)[4] == 0
}



# paint types: 
#  - s close & stroke path
#  - S stroke path
#  - f  fill (winding number)
#  - f* fill (even-odd)
#  - B  fill & stroke (winding)
#  - B* fill & stroke (even-odd)
#  - b  close, fill & stroke (winding)
#  - b* close, fill & stroke (even-odd)
#  - n end path without stroke or fill. used to define clipping path
gp_to_closed_paint_op <- function(gp) {
  if (identical(gp$rule, 'evenodd')) {
    'b*'
  } else {
    'b'
  }
}


# F1  = Helvetica            
# F2  = Helvetica-Bold       
# F3  = Helvetica-Oblique    
# F4  = Helvetica-BoldOblique
# F5  = Courier              
# F6  = Courier-Bold         
# F7  = Courier-Oblique      
# F8  = Courier-BoldOblique' 
# F9  = Times-Roman          
# F10 = Times-Bold           
# F11 = Times-Italic'        
# F12 = Times-BoldItalic     
# F13 = Symbol               
# F14 = ZapfDingbats         
gp_to_font_ref <- function(gp) {
  
  if (is.null(gp$fontface)) {
    face <- 'plain'
  } else if (is.numeric(gp$fontface)) {
    face <- c('plain', 'bold', 'italic', 'bold.italic')[gp$fontface]
  } else {
    face <- gp$fontface
  }
  
  stopifnot(face %in% c('plain', 'bold', 'italic', 'oblique', 'bold.italic'))
  
  
  if (is.null(gp$fontfamily)) {
    gp$fontfamily <- 'Helvetica'
  }
  
  switch(
    tolower(gp$fontfamily),
    sans      =,
    helvetica = {
      res <- switch(
        face,
        plain       = "F1",
        bold        = "F2",
        oblique     =,
        italic      = "F3",
        bold.italic = "F4",
        stop("Bad face: ", face)
      )
    },
    mono    =,
    courier = {
      res <- switch(
        face,
        plain       = "F5",
        bold        = "F6",
        oblique     =,
        italic      = "F7",
        bold.italic = "F8",
        stop("Bad face: ", face)
      )
    },
    seriv = ,
    times = {
      res <- switch(
        face,
        plain       = "F9",
        bold        = "F10",
        oblique     =,
        italic      = "F11",
        bold.italic = "F12",
        stop("Bad face: ", face)
      )
    },
    symbol = {
      res <- "F13"
    },
    zapfdingbats = {
      res <- "F14"
    },
    stop("Unknown font family: ", gp$fontfamily)
  )
  
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert graphics parameters to graphics state operators
#' 
#' These are values which can be expressed inline with the object.
#' Some options (such as 'CA' and 'ca') can only be defined as part
#' of a graphics state parameter dictionary.
#' 
#' Table 56 in ISO32000-2:2020(E)
#' 
#' @param gp named list 'gp' object as created by \code{\link{pgpar}()}
#' @return single string represeting graphics state
#' @noRd
#' @importFrom grDevices col2rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gp_to_gs_operators <- function(gp) {
  
  
  col   <- (grDevices::col2rgb(gp$col  , alpha = TRUE)/255) |> as.vector()
  fill  <- (grDevices::col2rgb(gp$fill , alpha = TRUE)/255) |> as.vector()
  
  col  <- glue::glue("{col[1]} {col[2]} {col[3]} RG")
  fill <- glue::glue("{fill[1]} {fill[2]} {fill[3]} rg")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Line config
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lineWidth  <- NULL
  lineCap    <- NULL
  lineJoin   <- NULL
  mitreLimit <- NULL
  dashArray  <- NULL
  
  lineWidth  <- glue::glue("{gp$lwd} w")
  mitreLimit <- glue::glue("{gp$linemitre} M")
  if (!is.null(gp$lineend)) {
    lineCap <- switch(
      gp$lineend,
      butt   = "0 J",
      round  = "1 J",
      square = "2 J",
      stop("gp$lineend not understood: ", gp$lineend)
    )
  }
  
  if (!is.null(gp$linejoin)) {
    lineJoin <- switch(
      gp$linejoin,
      mitre = "0 j",
      round = "1 j", 
      bevel = "2 j",
      stop("gp$linejoin not understood: ", gp$linejoin)
    )
  }
  
  if (!is.null(gp$lty)) {
    lty <- gp$lty
    if (is.character(lty)) {
      lty <- switch(
        lty,
        blank    = ,
        solid    = ,
        dashed   = ,
        dotted   = ,
        dotdash  = ,
        longdash = ,
        twodash  = ,
        stop("lty unknown: ", lty)
      )
    }
    if (lty == 0) stop("Blank lines not handled.  Use an alpha = 0 instead")
    if (lty < 1 || lty > 6) stop("lty out of range [1,6]: ", lty)
    dashArray <- c(
      "[] 0 d", 
      "[3] 0 d", 
      "[2] 0 d", 
      "[2 1] 0 d", 
      "[3 5] 0 d", 
      "[2 1 3] 0 d"
    )[lty]
    
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create string for inline graphics state operators
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- paste(c(
    col, fill, 
    lineWidth, lineCap, lineJoin, mitreLimit,
    dashArray
  ), collapse = "\n")
  
  
  res
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
  
  if (col[4] == 1 && fill[4] == 1 && gp$alpha == 1) {
    return(NULL)
  }
  
  pdf_dict(
    CA = col[4]  * gp$alpha, # stroke alpha 
    ca = fill[4] * gp$alpha  # fill alpha
  )
  
}








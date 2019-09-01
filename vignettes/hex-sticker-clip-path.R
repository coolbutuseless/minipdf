## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(minipdf)

## ------------------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate some lengths for sizing the hexagon
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
spacing <- 200
w <- sqrt(3) * spacing
h <- 2       * spacing

qh <- h/4
hw <- w/2


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup circles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
o1 <- 80
o2 <- 60
o3 <- 40
o4 <- 30

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create the PDF document
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doc <- PDFDocument$new(width = w, height = h, fontname = 'Courier-Bold')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Solid Background colour
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doc$rect(0, 0, w, h, fill = "#A84128", stroke = NULL)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Circles. Lots of circles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doc$circle(x = hw + o1, y = 2*qh + o1, r = o1    , fill = '#f8b918', stroke = NULL)
doc$circle(x = hw + o1, y = 2*qh - o1, r = o1    , fill = '#f8b918', stroke = NULL)
doc$circle(x = hw - o1, y = 2*qh + o1, r = o1    , fill = '#f8b918', stroke = NULL)
doc$circle(x = hw - o1, y = 2*qh - o1, r = o1    , fill = '#f8b918', stroke = NULL)

doc$circle(x = hw + o2, y = 2*qh + o2, r = o2-8.5, fill = '#ee9421', stroke = NULL)
doc$circle(x = hw + o2, y = 2*qh - o2, r = o2-8.5, fill = '#ee9421', stroke = NULL)
doc$circle(x = hw - o2, y = 2*qh + o2, r = o2-8.5, fill = '#ee9421', stroke = NULL)
doc$circle(x = hw - o2, y = 2*qh - o2, r = o2-8.5, fill = '#ee9421', stroke = NULL)

doc$circle(x = hw + o3, y = 2*qh + o3, r = o3-17 , fill = '#e75920', stroke = NULL)
doc$circle(x = hw + o3, y = 2*qh - o3, r = o3-17 , fill = '#e75920', stroke = NULL)
doc$circle(x = hw - o3, y = 2*qh + o3, r = o3-17 , fill = '#e75920', stroke = NULL)
doc$circle(x = hw - o3, y = 2*qh - o3, r = o3-17 , fill = '#e75920', stroke = NULL)

doc$circle(x = hw + o4, y = 2*qh + o4, r = 10    , fill = '#Af2a33', stroke = NULL)
doc$circle(x = hw + o4, y = 2*qh - o4, r = 10    , fill = '#Af2a33', stroke = NULL)
doc$circle(x = hw - o4, y = 2*qh + o4, r = 10    , fill = '#Af2a33', stroke = NULL)
doc$circle(x = hw - o4, y = 2*qh - o4, r = 10    , fill = '#Af2a33', stroke = NULL)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Label
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doc$text("minipdf", x = hw+5, y = 15, fill = '#ffffff', fontsize = 44)$
  rotate(30, hw+5, 15)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Write the PDF to file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doc$save("figures/logo-alternate.pdf")

## ----include = FALSE-----------------------------------------------------
system("convert -density 300 figures/logo-alternate.pdf -resize 100% -define png:exclude-chunks=date,time figures/logo-alternate.png")

## ----echo = FALSE, out.width = "45%"-------------------------------------
knitr::include_graphics("figures/logo-alternate.png")

## ------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define the x and y coords of a hexagon
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xs <- c(hw,  0,      0,     hw,      w,  w, hw)
ys <- c( 0, qh, 3 * qh, 4 * qh, 3 * qh, qh,  0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a polygonal clipping region
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clip_obj <- stream$clip_polygon(xs = xs, ys = ys)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Replace the default clipping path at position 1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doc$replace(clip_obj, position = 1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Write the PDF to file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doc$save("figures/logo-alternate-clipped.pdf")

## ----include = FALSE-----------------------------------------------------
system("convert -density 300 figures/logo-alternate-clipped.pdf -resize 100% -define png:exclude-chunks=date,time figures/logo-alternate-clipped.png")

## ----echo = FALSE, out.width = "45%"-------------------------------------
knitr::include_graphics("figures/logo-alternate-clipped.png")


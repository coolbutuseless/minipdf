## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "50%"
)

## ----setup---------------------------------------------------------------
library(minipdf)

## ------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialise the document
# Draw a pretty geometric pattern
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doc <- PDFDocument$new(width = 400, height = 400)

for (i in seq(0, 400, 20)) {
  doc$line(i    ,     0,   400,     i, stroke = '#00000020')
  doc$line(400  ,     i, 400-i,   400, stroke = '#00000020')
  doc$line(400-i,   400,     0, 400-i, stroke = '#00000020')
  doc$line(0    , 400-i,     i,     0, stroke = '#00000020')
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Draw a circle in the centre with some text around
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doc$circle(200, 200, 50, fill = "#12345680", stroke = NULL)

ncolours <- 40
pal <- rainbow(ncolours)
for (i in seq(ncolours)) {
  doc$text("                        #Rstats", 
               x = 200, y = 200, fill = pal[i])$
    rotate((1 - i)/ncolours * 360, 200, 200)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Write the PDF to file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doc$save("figures/example1.pdf")

## ----include = FALSE-----------------------------------------------------
system("convert -density 300 figures/example1.pdf -resize 100% -define png:exclude-chunks=date,time figures/example1.png")

## ----echo = FALSE--------------------------------------------------------
knitr::include_graphics("figures/example1.png")

## ------------------------------------------------------------------------
doc <- pdfdoc(width = 400, height = 400, fontname = 'Helvetica-Bold')

pal <- viridisLite::viridis(80)
i   <- 0
for (x in seq(20, 400, 40)) {
  for (y in seq(100, 400, 40)) {
    i <- i + 1
    doc$circle(x = x, y = y, r = 20, fill = pal[i], stroke = NULL)
  }
}

doc$text("minpdf", x = 95, y = 20, fontsize = 65, stroke = 'grey50', fill = 'lightblue', text_mode = 2)
doc$rect(0, 0, 400, 400, fill = NULL, stroke = 'grey70')

doc$save("figures/example2.pdf")

## ----include = FALSE-----------------------------------------------------
system("convert -density 300 figures/example2.pdf -resize 100% -define png:exclude-chunks=date,time figures/example2.png")

## ----echo = FALSE--------------------------------------------------------
knitr::include_graphics("figures/example2.png")


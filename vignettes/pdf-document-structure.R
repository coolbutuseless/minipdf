## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "50%"
)

## ----setup---------------------------------------------------------------
library(minipdf)

## ------------------------------------------------------------------------
stream$rect(x = 20, y = 30, width = 100, height = 200)

## ------------------------------------------------------------------------
PDFRect$new(x = 20, y = 30, width = 100, height = 200)

## ------------------------------------------------------------------------
doc <- PDFDocument$new()
new_rect  <- doc$add_rect(x = 20, y = 30, width = 100, height = 200)
new_rect

## ------------------------------------------------------------------------
dict(Page = "[1 0 R]")

## ------------------------------------------------------------------------
PDFDict$new(Type = "/Font")

## ------------------------------------------------------------------------
doc <- PDFDocument$new()
new_dict  <- doc$add_dict(Greg = "/StopSign")
new_dict

## ------------------------------------------------------------------------
dict(Page = "[1 0 R]", This = dict(Nested = "cool"))

## ------------------------------------------------------------------------
doc <- PDFDocument$new(width = 400, height = 100)

doc$add_text("#Rstats", x = 30, y = 20, fontsize = 100, fill = 'lightblue3')
doc$add_line(x1=20, y1=10, x2=380, y2=10, linewidth = 5, stroke = '#123456')

doc$save("figures/example0a.pdf")

## ----include = FALSE-----------------------------------------------------
system("convert -density 300 figures/example0a.pdf -resize 100% -define png:exclude-chunks=date,time figures/example0a.png")

## ----echo = FALSE--------------------------------------------------------
knitr::include_graphics("figures/example0a.png")

## ------------------------------------------------------------------------
doc <- PDFDocument$new(width = 400, height = 100)

the_text <- PDFText$new("#Rstats", x = 30, y = 20, fontsize = 100, fill = 'lightblue3')
the_line <- PDFLine$new(x1=20, y1=10, x2=380, y2=10, linewidth = 5, stroke = '#123456')

the_text$update(fill = 'darkgreen') # Adjust colour after creation

doc$add(the_text, the_line)

doc$save("figures/example0b.pdf")

## ----include = FALSE-----------------------------------------------------
system("convert -density 300 figures/example0b.pdf -resize 100% -define png:exclude-chunks=date,time figures/example0b.png")

## ----echo = FALSE--------------------------------------------------------
knitr::include_graphics("figures/example0b.png")

## ------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create individiaul R6 objects
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
the_text <- stream$text("#Rstats", x = 30, y = 20, fontsize = 100, fill = 'lightblue3')
the_line <- stream$line(x1=20, y1=10, x2=380, y2=10, linewidth = 5, stroke = '#123456')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adjust the colour of the text
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
the_text$update(fill = 'hotpink')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialise a document
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
doc <- pdfdoc(width = 400, height = 100, the_text, the_line)

doc$save("figures/example0c.pdf")

## ----include = FALSE-----------------------------------------------------
system("convert -density 300 figures/example0c.pdf -resize 100% -define png:exclude-chunks=date,time figures/example0c.png")

## ----echo = FALSE--------------------------------------------------------
knitr::include_graphics("figures/example0c.png")


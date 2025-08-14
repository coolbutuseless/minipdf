
<!-- README.md is generated from README.Rmd. Please edit that file -->

# minipdf <img src="man/figures/logo.png" align="right" height=230/>

<!-- badges: start -->

![](http://img.shields.io/badge/cool-useless-green.svg)
![](http://img.shields.io/badge/mini-verse-blue.svg)
[![R-CMD-check](https://github.com/coolbutuseless/minipdf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coolbutuseless/minipdf/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`minipdf` is a package for creating simple PDF documents.

## Installation

You can install the development version from
[GitHub](https://github.com/coolbutuseless/minipdf) with:

``` r
# install.packages("devtools")
devtools::install_github("coolbutuseless/minipdf")
```

## `Hello-world.pdf`

``` r
doc <- create_pdf(height = 400, width = 600)

N <- 400
xs <- sample(600, N, TRUE)
ys <- sample(400, N, TRUE)
rs <- sample(100, N, TRUE)
cs <- sample(colors(), N, TRUE)

for (i in seq_len(N)) {
  doc <- pdf_circle(doc, xs[i], ys[i], rs[i], col = NA, fill = cs[i], alpha = 0.2)
}

cs <- rainbow(400)
for (i in seq(1, 400, 10)) {
  doc <- pdf_line(doc, i, 0, 0, 400 - i, col = cs[i], alpha = 0.2)
}

doc <- pdf_translate(doc, 50, 0)

doc <- pdf_text(doc, "Hello", 20, 300, fontsize = 90, mode = 0, fill = 'black', 
                fontface = 'plain')

doc <- pdf_text(doc, "#RStats", 20, 200, fontsize = 90, mode = 1, col = 'hotpink', 
                fontface = 'bold.italic', lwd = 5)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_pdf(doc, "man/figures/example1.pdf")
```

<img src="man/figures/example1.png" width="75%" />

``` r
# write_pdf(doc) |> cat()
```

## References

- [PDF specification
  document](https://www.adobe.com/devnet/pdf/pdf_reference.html)
- [PDF: An Introduction for
  Programmers](http://preserve.mactech.com/articles/mactech/Vol.15/15.09/PDFIntro/index.html)
- [Make your own PDF
  file](https://blog.idrsolutions.com/2010/09/grow-your-own-pdf-file-part-1-pdf-objects-and-data-types/)

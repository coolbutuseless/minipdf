
<!-- README.md is generated from README.Rmd. Please edit that file -->

# minipdf <img src="man/figures/logo.png" align="right" height=230/>

<!-- badges: start -->

![](http://img.shields.io/badge/cool-useless-green.svg)
![](http://img.shields.io/badge/mini-verse-blue.svg)
[![R-CMD-check](https://github.com/coolbutuseless/minipdf/workflows/R-CMD-check/badge.svg)](https://github.com/coolbutuseless/minipdf/actions)
[![R-CMD-check](https://github.com/coolbutuseless/minipdf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coolbutuseless/minipdf/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`minipdf` is a package for creating simple, single-page PDF documents.

## Installation

You can install the development version from
[GitHub](https://github.com/coolbutuseless/minipdf) with:

``` r
# install.packages("devtools")
devtools::install_github("coolbutuseless/minipdf")
```

See the online documentation
[here](https://coolbutuseless.github.io/package/minipdf/index.html)
(thanks to [pkgdown](https://cran.r-project.org/package=pkgdown))

## `Hello-world.pdf`

``` r
# doc <- PDFDocument$new(width = 200, height = 60, fontname = 'Helvetica-Bold')
# doc$rect(0, 0, 200, 60, fill = '#123456', stroke = NULL)
# doc$text("Hello World!", x = 10, y = 15, fontsize = 20, fill = 'white')
# doc$line(0, 10, 200, 10, stroke = 'grey80')
# doc$save("man/figures/helloworld.pdf")
```

## References

- [PDF specification
  document](https://www.adobe.com/devnet/pdf/pdf_reference.html)
- [PDF: An Introduction for
  Programmers](http://preserve.mactech.com/articles/mactech/Vol.15/15.09/PDFIntro/index.html)
- [Make your own PDF
  file](https://blog.idrsolutions.com/2010/09/grow-your-own-pdf-file-part-1-pdf-objects-and-data-types/)

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PDFStreamX.R
\docType{data}
\name{ptag}
\alias{ptag}
\title{Helper for building PDF Stream Objects e.g. Rect, Lines}
\format{
An object of class \code{list} of length 7.
}
\usage{
ptag
}
\description{
For documentation, see manual for the related R6 function e.g. `PDFRect`
}
\examples{
\dontrun{
ptag$rect(x = 0, y = 0, width = 100, height = 100)
ptag$circle(x = 100, y = 100, r = 10, fill = 'black')
}

}
\seealso{
PDFText PDFRect PDFLine PDFPolyline PDFPolygon PDFCircle PDFCustom
PDFClipRect PDFClipPolygon
}
\keyword{datasets}

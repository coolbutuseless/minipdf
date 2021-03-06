% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PDFStreamX.R
\name{PDFClipRect}
\alias{PDFClipRect}
\title{PDF ClipRect stream object creator}
\description{
PDF ClipRect stream object creator

PDF ClipRect stream object creator
}
\details{
This is the R6 class representing the ClipRect stream object.
}
\section{Super class}{
\code{\link[minipdf:PDFStream]{minipdf::PDFStream}} -> \code{PDFClipRect}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{PDFClipRect$new()}}
\item \href{#method-get_geom_spec}{\code{PDFClipRect$get_geom_spec()}}
\item \href{#method-clone}{\code{PDFClipRect$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="as_character">}\href{../../minipdf/html/PDFStream.html#method-as_character}{\code{minipdf::PDFStream$as_character()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="as_object">}\href{../../minipdf/html/PDFStream.html#method-as_object}{\code{minipdf::PDFStream$as_object()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="clip_polygon">}\href{../../minipdf/html/PDFStream.html#method-clip_polygon}{\code{minipdf::PDFStream$clip_polygon()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="clip_rect">}\href{../../minipdf/html/PDFStream.html#method-clip_rect}{\code{minipdf::PDFStream$clip_rect()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="copy">}\href{../../minipdf/html/PDFStream.html#method-copy}{\code{minipdf::PDFStream$copy()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="fill">}\href{../../minipdf/html/PDFStream.html#method-fill}{\code{minipdf::PDFStream$fill()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="fontsize">}\href{../../minipdf/html/PDFStream.html#method-fontsize}{\code{minipdf::PDFStream$fontsize()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="get_attrib">}\href{../../minipdf/html/PDFStream.html#method-get_attrib}{\code{minipdf::PDFStream$get_attrib()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="get_transform_spec">}\href{../../minipdf/html/PDFStream.html#method-get_transform_spec}{\code{minipdf::PDFStream$get_transform_spec()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="linetype">}\href{../../minipdf/html/PDFStream.html#method-linetype}{\code{minipdf::PDFStream$linetype()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="linewidth">}\href{../../minipdf/html/PDFStream.html#method-linewidth}{\code{minipdf::PDFStream$linewidth()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="print">}\href{../../minipdf/html/PDFStream.html#method-print}{\code{minipdf::PDFStream$print()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="rotate">}\href{../../minipdf/html/PDFStream.html#method-rotate}{\code{minipdf::PDFStream$rotate()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="scale">}\href{../../minipdf/html/PDFStream.html#method-scale}{\code{minipdf::PDFStream$scale()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="stroke">}\href{../../minipdf/html/PDFStream.html#method-stroke}{\code{minipdf::PDFStream$stroke()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="text_mode">}\href{../../minipdf/html/PDFStream.html#method-text_mode}{\code{minipdf::PDFStream$text_mode()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="translate">}\href{../../minipdf/html/PDFStream.html#method-translate}{\code{minipdf::PDFStream$translate()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="minipdf" data-topic="PDFStream" data-id="update">}\href{../../minipdf/html/PDFStream.html#method-update}{\code{minipdf::PDFStream$update()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Initialize a stream object representing a clipping rectangle
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PDFClipRect$new(x, y, width, height, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x, y, width, height}}{rectangle definition}

\item{\code{...}}{initial named attributes of this object}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_geom_spec"></a>}}
\if{latex}{\out{\hypertarget{method-get_geom_spec}{}}}
\subsection{Method \code{get_geom_spec()}}{
Fetch the character representation of this geometry
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PDFClipRect$get_geom_spec()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PDFClipRect$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}

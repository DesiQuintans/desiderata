% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_tools.R
\name{col2hsv}
\alias{col2hsv}
\title{Convert Hex colours to HSV}
\usage{
col2hsv(hexcol, which = "hsv")
}
\arguments{
\item{hexcol}{(Character) A vector of hex colours.}

\item{which}{(Character) Which variables to return. \code{"HSV"} will return Hue,
Saturation, and Value. Any combination of these letters will return those
columns, so \code{"HV"} will return only Hue and Value.}
}
\value{
A dataframe if more than one column is requested in \code{which},
otherwise a numeric vector.
}
\description{
Convert Hex colours to HSV
}
\section{Authors}{

\itemize{
\item Desi Quintans (\url{http://www.desiquintans.com})
}
}

\examples{
col2hsv(c("#FFD8B1", "#808000"))

#>          h         s         v
#> 0.08333333 0.3058824 1.0000000
#> 0.16666667 1.0000000 0.5019608

col2hsv(c("#FFD8B1", "#808000"), which = "hv")

#>          h         v
#> 0.08333333 1.0000000
#> 0.16666667 0.5019608

}

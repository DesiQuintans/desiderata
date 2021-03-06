% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r_tools.R
\name{dots_char}
\alias{dots_char}
\title{Convert dots (...) to a character vector or a string}
\usage{
dots_char(..., collapse = NULL)
}
\arguments{
\item{...}{(Dots) Elements in dots.}

\item{collapse}{(Character or \code{NULL}) The elements of \code{...} will be \code{paste}d
together with this string between them.}
}
\value{
A character vector. If \code{collapse = NULL}, returns each separate item in
\code{...} as a separate element of a character vector. If \code{collapse = "-"}, for
example, returns a single string where all items in \code{...} are joined together
with \code{-}.
}
\description{
Convert dots (...) to a character vector or a string
}
\examples{
dots_char(return, this, as, a, vector)

#> [1] "return" "this" "as" "a" "vector" 

dots_char(return, this, as, a, single, string, collapse = "_")

#> [1] "return_this_as_a_single_string"

}

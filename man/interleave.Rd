% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string_tools.R
\name{interleave}
\alias{interleave}
\title{Interleave one string with another}
\usage{
interleave(str, insert = "\\n", split = character(0))
}
\arguments{
\item{str}{(Character) The string to be split into sections.}

\item{insert}{(Character) The string to be inserted between sections.}

\item{split}{(Character) Regular expression to split \code{str} by. If its length is
zero (the default), it will split at every character.}
}
\value{
A Character vector.
}
\description{
Interleave a string with another string. By default, interleaves newlines (\verb{\\\\n})
between each letter to help format a compact letter display.
}
\examples{
interleave(c("hello", "hey"), "-")

## [1] "h-e-l-l-o" "h-e-y" 

interleave(c("hello", "hey"), insert = "|_|", split = "el")
## [1] "h|_|lo" "hey"  

interleave(c(1234, 9876), "-")

## [1] "1-2-3-4" "9-8-7-6"

}

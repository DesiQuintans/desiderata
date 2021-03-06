% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string_tools.R
\name{rev_sentence}
\alias{rev_sentence}
\title{Reverse the order of words in a string}
\usage{
rev_sentence(vec, split = " |_", join = " ")
}
\arguments{
\item{vec}{(Character) The vector to reverse.}

\item{split}{(Character) A regular expression that determines where \code{vec} will be
split into separate words. By default, splits at spaces and underscores.}

\item{join}{(Character) A string that will be inserted between the words.}
}
\value{
A character vector that is the same length as \code{vec}.
}
\description{
Reverse the order of words in a string
}
\section{Authors}{

\itemize{
\item Desi Quintans (\url{http://www.desiquintans.com})
}
}

\examples{
vec <- c("Lorem ipsum dolor",
         "sit amet, consectetur",
         "adipiscing elit, sed",
         "do eiusmod tempor",
         "incididunt ut labore",
         "et dolore magna",
         "aliqua.")

rev_sentence(vec)

## [1] "dolor ipsum Lorem"  "consectetur amet, sit"  "sed elit, adipiscing"  
## [4] "tempor eiusmod do"  "labore ut incididunt"  "magna dolore et"  "aliqua."   

}

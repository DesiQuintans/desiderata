% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string_tools.R
\name{collapse_vec}
\alias{collapse_vec}
\title{Collapse a vector into a string}
\usage{
collapse_vec(..., wrap = "'", collapse = ", ", unique = TRUE)
}
\arguments{
\item{...}{(...) Vectors that will be concatenated and coerced to Character.}

\item{wrap}{(Character) Placed at the left and right sides of each vector element.}

\item{collapse}{(Character) Placed between each element of the original vector(s).}

\item{unique}{(Logical) If \code{TRUE}, duplicate entries in \code{...} will be removed.}
}
\value{
A string.
}
\description{
Take the elements of multiple vectors and concatenate them into one long string.
}
\section{Authors}{

\itemize{
\item Desi Quintans (\url{http://www.desiquintans.com})
}
}

\examples{
\dontrun{
collapse_vec(month.abb, wrap = "-", collapse = ", ")
#> [1] "-Jan-, -Feb-, -Mar-, -Apr-, -May-, -Jun-, -Jul-, -Aug-, -Sep-, -Oct-, -Nov-, -Dec-"
}

}

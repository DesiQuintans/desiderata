% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataframe_tools.R
\name{drop_empty_cols}
\alias{drop_empty_cols}
\title{Drop 'empty' columns in a dataframe}
\usage{
drop_empty_cols(
  df,
  from = 1,
  to = NULL,
  cols = NULL,
  regex = "^$",
  report = FALSE
)
}
\arguments{
\item{df}{(Dataframe) A dataframe.}

\item{from, to}{(Numeric or \code{NULL}) The start and end of a continuous range of columns
that will be subsetted from \code{df}. If \code{to} is \code{NULL}, it defaults to the last
column in \code{df} so that \verb{from = 2, to = NULL} is the same as \code{2:length(df)}.}

\item{cols}{(Numeric or \code{NULL}) A numeric vector of the columns to consider. This
allows you to select non-contiguous columns. If the \code{cols} argument is being used
(not-\code{NULL}), \code{from} and \code{to} will be ignored.}

\item{regex}{(Character) A regex pattern that matches a value that should be considered
'empty'.}

\item{report}{(Logical) If \code{TRUE}, print a Message with the names of the empty columns
that were dropped.}
}
\value{
A subset of \code{df} with all empty columns removed.
}
\description{
Deletes columns from a dataframe if they are 'empty'. A column is empty when every
single row is \code{NA}, \code{NULL}, \code{""}, or matches a regular expression.
}
\section{Authors}{

\itemize{
\item Desi Quintans (\url{http://www.desiquintans.com})
}
}

\examples{
data <- data.frame(a = c(1, 2, 3),
                   b = c(0, 0, 0),
                   c = c(1, 1, 0),
                   d = c("", "", ""),
                   e = c("moo", "baa", "woof"))

#> a b c d    e
#> 1 0 1    moo
#> 2 0 1    baa
#> 3 0 0    woof

drop_empty_cols(data)

#> a b c   e
#> 1 0 1 moo
#> 2 0 1 baa
#> 3 0 0 woof

drop_empty_cols(data, regex = "moo|baa|woof")

#> a b c
#> 1 0 1
#> 2 0 1
#> 3 0 0

drop_empty_cols(data, regex = "moo|baa|woof", report = TRUE)

#> Empty cols dropped: d, e
#> a b c
#> 1 0 1
#> 2 0 1
#> 3 0 0

}

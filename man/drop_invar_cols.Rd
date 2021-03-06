% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataframe_tools.R
\name{drop_invar_cols}
\alias{drop_invar_cols}
\title{Drop invariant columns from a dataframe}
\usage{
drop_invar_cols(
  df,
  from = 1,
  to = NULL,
  cols = NULL,
  nearest = NULL,
  dir = NULL
)
}
\arguments{
\item{df}{(Dataframe) A dataframe.}

\item{from, to}{(Numeric or \code{NULL}) The start and end of a continuous range of columns
that will be used. If \code{to} is \code{NULL}, it defaults to the last column in \code{df} so
that \verb{from = 2, to = NULL} is the same as \code{2:length(df)}.}

\item{cols}{(Numeric or \code{NULL}) A numeric vector of the columns to consider. This
allows you to select non-contiguous columns. If the \code{cols} argument is being used
(not-\code{NULL}), \code{from} and \code{to} will be ignored.}

\item{nearest}{(Numeric or \code{NULL}) For numeric columns, this is the common value that
all numbers will be rounded to. The default \code{NULL} uses the \code{mean()} of each
column as the rounding target.}

\item{dir}{(Character or \code{NULL}) Controls the rounding function used. Leave as \code{NULL}
to round up and down. Use \code{"up"} to round up only. Use \code{"down"} to round down only.}
}
\value{
A copy of \code{df} with all invariant columns removed.
}
\description{
Deletes columns from a dataframe if they do not vary. For \code{character} and \code{factor}
columns, this means that every row of the column contains exactly the same string.
For \code{numeric} columns, the numbers are rounded to a nearest common value and then
checked to see if every rounded number is the same.
}
\section{Authors}{

\itemize{
\item Desi Quintans (\url{http://www.desiquintans.com})
}
}

\examples{
df <- data.frame(stringsAsFactors=FALSE,
         char_invar = c("A", "A", "A", "A", "A"),
           char_var = c("A", "A", "A", "B", "A"),
          num_invar = c(1L, 1L, 1L, 1L, 1L),
         num_mean_0 = c(0, -0.1, 0.1, 0.01, -0.01),
            num_var = c(0, 0.2, 0.8, 0.03, 0.4)
      )

df

#>   char_invar char_var num_invar num_mean_0 num_var
#> 1          A        A         1       0.00    0.00
#> 2          A        A         1      -0.10    0.20
#> 3          A        A         1       0.10    0.80
#> 4          A        B         1       0.01    0.03
#> 5          A        A         1      -0.01    0.40


drop_invar_cols(df)

#>   char_var num_var
#> 1        A    0.00
#> 2        A    0.20
#> 3        A    0.80
#> 4        B    0.03
#> 5        A    0.40

}

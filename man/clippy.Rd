% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interactive_programming.R
\name{clippy}
\alias{clippy}
\title{Copy a dataframe, vector, or the result of an expression to the clipboard}
\usage{
clippy(x = NULL)
}
\arguments{
\item{x}{(An expression, vector, dataframe, or \code{NULL}) The thing to copy to the
clipboard. If \code{NULL}, the \code{.Last.value} will be copied. If x is a dataframe, it will
be copied with column names but not row names. If x is something else (a vector, for
example) it will be coerced into a one-column dataframe and copied \strong{without} column
names or row names. Char and factor in x will not be surrounded by quotes in the
clipboard.}
}
\value{
Invisibly returns the contents of the clipboard. If \code{.Last.value} is displayed
in RStudio's 'Environment' tab, you'll see its value change.
}
\description{
Tested on Windows.
}
\section{Authors}{

\itemize{
\item Desi Quintans (\url{http://www.desiquintans.com})
\item Darwin PC (\url{https://stackoverflow.com/users/2543306})
\item dracodoc (\url{https://gist.github.com/dracodoc})
}
}

\section{Source}{

\itemize{
\item \url{https://stackoverflow.com/a/28845828/5578429}
\item \url{https://gist.github.com/dracodoc/74e5d2042efec0dfd9fcdbe6d65cf7e2}
}
}

\examples{
clippy(mtcars)

# Clipboard contents:
# mpg  cyl disp  hp   drat  wt     qsec   vs  am  gear  carb
# 21   6   160   110  3.9   2.62   16.46  0   1   4     4
# 21   6   160   110  3.9   2.875  17.02  0   1   4     4
# ...

clippy(iris$Petal.Length)

# Clipboard contents:
# 1.4
# 1.4
# 1.3
# ...

clippy(colnames(iris))

# Clipboard contents:
# Sepal.Length
# Sepal.Width
# Petal.Length
# Petal.Width
# Species

}

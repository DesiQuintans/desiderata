% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_tools.R
\name{theme_desi_base}
\alias{theme_desi_base}
\title{My personally-preferred minimal base plot theme}
\usage{
theme_desi_base(...)
}
\arguments{
\item{...}{(Character) Arguments to send to \code{ggplot2::theme()}.}
}
\value{
\code{ggplot2} theme objects.
}
\description{
\code{theme_bw()} with no grid, no borders, legend at the top, and no legend title.
}
\section{Authors}{

\itemize{
\item Desi Quintans (\url{http://www.desiquintans.com})
}
}

\examples{
# ggplot(iris, aes(x = Petal.Length, y = Sepal.Length)) +
#     geom_point() +
#     theme_desi_base()

}

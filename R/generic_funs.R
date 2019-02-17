# Generic functions/methods

# howmany ----------------------------------------------------------------------


#' Count the number of unique values
#'
#' @param x (Object) The object to count.
#'
#' @export
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
howmany <- function(x, ...) {
    UseMethod("howmany", x)
}



#' @describeIn howmany Returns the number of unique values in `x`.
#'
#' @param ... `default:` Additional vectors that will be combined with `x` using `c()`.
#'
#' @export
#'
#' @examples
#' ## --- Default method ---
#'
#' howmany(rep(letters, 3))
#' #> 26
#'
#' howmany(letters, LETTERS)
#' #> 52
#'
#' @md
howmany.default <- function(x, ...) {
    length(unique(c(x, ...)))
}



#' @describeIn howmany Returns a dataframe showing the number of unique values in
#'    each column of `x`.
#'
#' @param ... `data.frame:` Column names or positions that are passed to `dplyr::select()`.
#'
#' @export
#'
#' @examples
#' ## --- Data.frame method ---
#'
#' howmany(mtcars)
#'
#' #> mpg cyl disp hp drat wt qsec vs am gear carb
#' #>  25   3   27 22   22 29   30  2  2    3    6
#'
#' howmany(mtcars, -(mpg:disp))
#'
#' #> hp drat wt qsec vs am gear carb
#' #> 22   22 29   30  2  2    3    6
#'
#' howmany(mtcars, drat)
#'
#' #> drat
#' #>   22
#'
#' @md
howmany.data.frame <- function(x, ...) {
    # NOTE: This could run 3x faster with
    #     apply(df, 2, function(x) length(unique(x)))
    # but being able to use tidyselectors in the function is more convenient.

    dots <- dplyr::quos(...)

    # Has to be like this so that ... is handled predictably for the user
    if (length(dots) == 0) {
        cols <- dplyr::select(x, dplyr::everything())
    } else {
        cols <- dplyr::select(x, !!! dots)
    }

    dplyr::summarise_all(cols, howmany)
}



#' @describeIn howmany Coerces `x` to `data.frame` and passes it to `data.frame` method.
#'
#' @param ... `table:` Column names or positions that are passed to `dplyr::select()`.
#'
#' @export
#'
#' @examples
#' ## --- Table method ---
#'
#' class(Titanic)
#' #> [1] "table"
#'
#' howmany(Titanic)
#'
#' #> Class Sex Age Survived Freq
#' #>     4   2   2        2   22
#'
#' @md
howmany.table <- function(x, ...) {
    df <- as.data.frame(x, stringsAsFactors = FALSE)
    howmany.data.frame(df, ...)
}

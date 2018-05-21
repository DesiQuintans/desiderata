# Functions for working with dates.

#' Find the current month number relative to a starting date
#'
#' Sometimes it's helpful to be able to break down elapsed time in more granular
#' units, like being able to say, "I am in the 15th month of this experiment."
#'
#' @param from (Character or Numeric) A date in the ISO-8601 form `"YYYY-MM-DD"`, or a
#'     single number for the year which implies `YYYY-01-01`.
#' @param to (Character) A date in the ISO-8601 form `YYYY-MM-DD`.
#'
#' @return A numeric vector for the number of months elapsed.
#' @export
#'
#' @examples
#' # These are the same thing.
#' consecutive_month(2015, "2016-02-04")
#' consecutive_month("2015-01-01", "2016-02-04")
#' #> [1] 14
#'
#' consecutive_month("2015-02-01", "2016-02-04")
#' #> [1] 13
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
consecutive_month <- function(from, to) {
    if (is.numeric(from)) {from <- paste0(from, "-01-01")}
    startdate <- lubridate::ymd(from)

    month_offset <- (lubridate::year(to) - lubridate::year(startdate)) * 12
    return(month_offset + lubridate::month(to) - (lubridate::month(startdate) - 1))
}
<<<<<<< HEAD
=======


con
>>>>>>> c1bcf2863fcf7d93c2fb4ab2235ae7d9e5a6b1e5

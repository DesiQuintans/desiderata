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



#' Find the number of weeks between two dates
#'
#' Sometimes it's helpful to be able to break down elapsed time in more granular
#' units, like being able to say, "This experiment has been running for 
#' 15 weeks". It may also be helpful to say, "It has been 10 calendar weeks since
#' this happened". This function does both of those.
#'
#' @param from (Character or Numeric) A date in the ISO-8601 form `"YYYY-MM-DD"`, or a
#'     single number for the year which implies `YYYY-01-01`.
#' @param to (Character) A date in the ISO-8601 form `YYYY-MM-DD`.
#' @param tz (Character or `NULL`) A timezone name such as "Australia/Sydney". 
#'     Use `OlsonNames()` for a list of these.
#' @param iso (Logical) If `FALSE`, returns a naive estimate of weeks elapsed. 
#'     With R's `difftime()`, it's the same as dividing the days elapsed by 7.
#'     Use `round()`, `ceiling()`, or `floor()` to simplify the result if desired.
#'     \cr
#'     If `TRUE`, returns the number of isoweeks (calendar weeks starting on 
#'     Mondays) between the two dates. This means that yesterday (Sunday) and 
#'     today (Monday) fall across two isoweeks even though they're only one 
#'     day apart.
#'
#' @return A numeric vector for the number of weeks elapsed.
#' @export
#'
#' @examples
#' consecutive_week("2022-01-01", "2022-05-02", iso = FALSE)
#' #> [1] 17.28571
#' 
#' # Same dates, but they fall over multiple calendar weeks.
#' consecutive_week("2022-01-01", "2022-05-02", iso = TRUE)
#' #> [1] 4
#' 
#' # Note that consecutive days can spill over isoweeks.
#' consecutive_week("2022-05-01", "2022-05-02", iso = TRUE)
#' #> [1] 2
#'
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
consecutive_week <- function(from, to, tz = NULL, iso = FALSE) {
    if (is.numeric(from)) {from <- paste0(from, "-01-01")}
    if (is.numeric(to))   {to <- paste0(to, "-01-01")}
    
    startdate <- lubridate::ymd(from, tz = tz)
    enddate   <- lubridate::ymd(to, tz = tz)
    
    if (iso == FALSE) {
        # Just return the number of weeks elapsed. Note backwards order of 
        # enddate and startdate, which difftime likes for some reason.
        difftime(enddate, startdate, tz, units = "weeks") |>
            as.numeric()
    } else {
        # Return the number of isoweeks that have elapsed. This is calendar 
        # weeks, starting on Monday. Isoweeks can also spill over calendar 
        # years, which makes this code so tricky.
        
        date_df <- 
            dplyr::tibble(dates = seq(startdate, enddate, by = "days")) |> 
            dplyr::mutate(yr = lubridate::year(dates),
                          isowk = lubridate::isoweek(dates))
        
        # The first isoweek, which can be 1-7 rows long depending on startdate.
        week1 <- 
            dplyr::filter(date_df, 
                          yr    == dplyr::first(yr),
                          isowk == dplyr::first(isowk))
        
        # The other weeks
        other_weeks <- 
            date_df |> 
            # Remove the first isoweek from the table
            dplyr::anti_join(week1, by = c("dates", "yr", "isowk"))
        
        # Then split the isowk vector into slices with 7 days in each.
        # The number of slices + 1 (for the week in week1) is the number
        # of consecutive isoweeks between the two dates.
        week_splits <- desiderata::split_size(other_weeks$isowk, size = 7)
        
        return(length(week_splits) + 1)
    }
}



#' Round a duration of days to other units
#'
#' Convert a duration of days into approximate months or years, rounding it to a 
#' nearest value if desired. Note that time durations larger than the week 
#' are only approximate.
#'
#' @param num (Numeric/Integer) Time durations you want to convert.
#' @param from (Character) The units of time that `num` is in, e.g. `"days"`.
#' @param to (Character) The units of time to convert to, e.g. `"weeks"`.
#' @param nearest (Numeric/Integer or `NULL`) The closest number to round the 
#'    results to, e.g. `1` to round to integers. If `NULL`, no rounding is done.
#' @param dir (Character or `NULL`) Controls the rounding function used if 
#'    `nearest` is not `NULL`. If omitted or `"both"`, the `round()` function is 
#'    used. If `"up"`, `ceiling()` is used. If `"down"`, `floor()` is used.
#'
#' @return A numeric vector. 
#' @export
#'
#' @examples
#' round_to_duration(134, "days", "weeks")
#' #> [1] 19.14286
#' 
#' # Note that this is the same as a naive calculation; if you were converting 
#' # days to months, then it's just days divided by 30.4375 (the average number 
#' # of days per month).
#' round_to_duration(134, "days", "months")
#' #> [1] 4.402464
#' 134 / 30.4375
#' #> [1] 4.402464
#' 
#' # You can round the output too. This is useful if you want to express a time
#' # duration like, "1.5 months".
#' round_to_duration(134, "days", "weeks", nearest = 0.5, dir = "up")
#' #> [1] 19.5
#' round_to_duration(134, "days", "weeks", nearest = 0.5, dir = "down")
#' #> [1] 19
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
round_to_duration <- function(num, from, to, nearest = NULL, dir = NULL) {
    from_dur <- lubridate::duration(num, units = from)
    converted <- as.numeric(from_dur, to)

    if (is.null(nearest)) {
        return(converted)
    } else {
        rounded <- desiderata::round_to_nearest(converted, to = nearest, dir = dir)
        return(rounded)
    }
}

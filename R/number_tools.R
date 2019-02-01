# Extra functions that do more maths and number-related things.


#' Mode of a vector (numeric/character/factor)
#'
#' There is no built-in function to find the mode of something. This function can find
#' the mode of a numeric, character, or factor vector. It will return multiple values in
#' the case of a multi-modal dataset. In the case of a numeric vector, it can return a
#' single value that is the mean of the modes.
#'
#' If all values are unique, it will return **all** of the values.
#'
#' @param x (Char/Numeric/Factor) A vector.
#' @param na.rm (Logical) If `TRUE`, NAs will be silently removed.
#' @param ties (Logical) If `TRUE` and `x` has multiple modes (e.g. `c(2, 2, 1, 1)`),
#'    return all of modes that were found (`2, 1`). If `FALSE`, only the first mode
#'    (the one first appearing in `x`) will be returned (`2`).
#' @param mean (Logical) If `TRUE`, return the average of all the modes. Only makes sense
#'   for a numeric vector. Only the first mode is returned if `ties == FALSE`,
#'
#' @return A vector of the mode value(s).
#' @export
#'
#' @examples
#' vec <- c(1, 2, 3, 4, 4, 4, 3, 3, NA, NA, NA)
#'
#' Mode(vec)
#' #> [1]  3  4 NA
#'
#' Mode(vec, na.rm = TRUE)
#' #> [1] 3 4
#'
#' Mode(vec, na.rm = FALSE, mean = TRUE)
#' #> [1] NA
#'
#' Mode(vec, na.rm = TRUE, mean = TRUE)
#' #> [1] 3.5
#'
#' Mode(1:4)
#' #> [1] 1 2 3 4
#'
#' Mode(1:4, ties = FALSE)
#' #> [1] 1
#'
#' Mode(1:4, mean = TRUE)
#' #> [1] 2.5
#'
#' @section Authors:
#' - Ken Williams (<https://stackoverflow.com/users/169947/ken-williams>)
#' - jprockbelly (<https://stackoverflow.com/users/1502898/jprockbelly>)
#' - digEmAll (<https://stackoverflow.com/users/316644/digemall>)
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @section Source:
#' <https://stackoverflow.com/a/8189441/5578429>
#'
#' @md
Mode <- function(x, na.rm = FALSE, ties = FALSE, mean = FALSE) {
    if (na.rm) {
        x = x[!is.na(x)]
    }

    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    result <- ux[tab == max(tab)]

    if (mean == TRUE) {
        result <- mean(result)
    }

    if (ties == FALSE) {
        result <- result[1]
    }

    return(result)
}



#' Geometric mean of a vector
#'
#' The geometric mean is useful when your values are imbalanced by outliers. Daniel
#' McNichol wrote [a very approachable
#' article](https://towardsdatascience.com/on-average-youre-using-the-wrong-average-geometric-harmonic-means-in-data-analysis-2a703e21ea0)
#' about this topic.
#'
#' This function is by Paul McMurdie and is "vectorized, zero- and NA-tolerant [...]
#' checks for any negative values, and returns a more informative and appropriate NaN
#' respecting that geometric mean is not defined for negative values (but is for zeros)."
#'
#' @param x (Numeric) A vector.
#' @param na.rm (Logical) If TRUE, NAs will be silently removed.
#'
#' @return A numeric vector of length 1.
#' @export
#'
#' @examples
#' vec <- c(1, 3, 9, 27, 81, 243, 729)
#'
#' mean(vec)
#' #> [1] 156.1429
#'
#' geomean(vec)
#' #> [1] 27
#'
#' @section Authors:
#' - Paul McMurdie (<https://stackoverflow.com/users/935950/paul-mcmurdie>)
#'
#' @section Source:
#' <https://stackoverflow.com/a/25555105/5578429>
#' @md
geomean <- function(x, na.rm = TRUE) {
    exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}




#' Round a number to a fixed decimal place length
#'
#' R's `round()` and `signif()` functions drop trailing zeros because they are not
#' significant, but it's sometimes important to keep them for formatting i.e. tables and
#' graphs. This function does that.
#'
#' @param num (Numeric) A vector of numbers.
#' @param places (Numeric) The number of decimal places to keep.
#'
#' @return A Character vector of rounded numbers.
#' @export
#'
#' @examples
#' vec <- c(1.739006, 2, -1.4, 1.05, 1.90, 3.826)
#' rounded_vec <- round_to_places(vec, 2)
#'
#' str(rounded_vec)
#' #> chr [1:6] "1.70" "2.00" "-1.40" "1.00" "1.90" "3.80"
#'
#' @section Authors:
#' - Kristine Yu (<https://kmyu.wordpress.com>)
#'
#' @section Source:
#' <https://kmyu.wordpress.com/2011/01/11/formatting-numbers-for-printing-in-r-rounding-and-trailing-zeroes/>
#'
#' @md
round_to_places <- function(num, places = 2) {
    warning("round_to_places() is deprecated.\nInstead, use the base function round() with 'digits' arg.")
    return(
        round(num, digits = places)
    )
}



#' Round numbers to a nearest "pretty" value
#'
#' It's sometimes useful to round numbers to a predictable range, e.g. when preparing
#' graph axes or perhaps for filtering or presenting data. This function can round numbers
#' to the nearest, say, 0.5.
#'
#' @param num (Numeric) A vector of numbers.
#' @param to (Numeric) What interval should `num` be rounded to?
#' @param dir (Optional Char: `"up"` or `"down"` or omitted) Controls the rounding
#'   function used. If omitted, the `round()` function is used. If `"up"`, `ceiling()` is
#'   used. If `"down"`, `floor()` is used.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' vec <- c(1.739006, 2, -1.4, 1.05, 1.90, 3.826)
#' round_to_nearest(vec, 0.5)
#' #> [1]  1.5  2.0 -1.5  1.0  2.0  4.0
#'
#' @section Authors:
#' - Koshke (<https://stackoverflow.com/users/314020/kohske>)
#'
#' @section Source:
#' <https://stackoverflow.com/a/8665247/5578429>
#'
#' @md
round_to_nearest <- function(num, to, dir = NULL) {
    # Useful for formatting graph scales by rounding to a specified "pretty" value.
    # http://stackoverflow.com/questions/8664976/r-round-to-nearest-5-or-1

    if (is.null(dir)) return(round(num / to) * to)

    if (dir == "up") return(ceiling(num / to) * to)

    if (dir == "down") return(floor(num / to) * to)
}




#' Seed the random number generator with a character string (or any object)
#'
#' Set the random number generator's seed using a string if you want to be extra cute and
#' use your cats' names like I do. This function can actually generate a seed from any R
#' object, so you could even feed it a whole dataframe if you felt like it. (Requires the
#' ['digest'](https://cran.r-project.org/web/packages/digest/index.html) package.)
#'
#' @param seed (Any) Any object.
#'
#' @return `NULL`.
#' @export
#'
#' @examples
#' # set_seed_any("Snake... Do you think love can bloom, even on a battlefield?")
#'
#' # set_seed_any(iris)
#'
#' @section Authors:
#' - Ben Bolker (<https://stackoverflow.com/users/190277/ben-bolker>)
#'
#' @section Source:
#' <https://stackoverflow.com/a/10913336/5578429>
#'
#' @md
set_seed_any <- function(seed) {
    digest_installed <- "digest" %in% rownames(utils::installed.packages())
    if (digest_installed == FALSE) {
        stop("The 'digest' package needs to be installed.")
    }

    hexval <- paste0("0x", digest::digest(seed, "crc32"))
    intval <- utils::type.convert(hexval) %% .Machine$integer.max
    set.seed(intval)
}



#' Calculate degree-days
#'
#' > "In a nutshell: heating degree days are a measure of how much (in degrees), and for
#' how long (in days), the air temperature was below a certain level."
#' > --- Martin Bromley, <http://www.degreedays.net/introduction>
#'
#' This function exposes two calculation methods that were taken from
#'
#' Herms, Daniel A. 2013. “Using Degree-Days and Plant Phenology to Predict Pest
#' Activity.” IPM of Midwest Landscapes: Tactics and Tools for IPM. 2013.
#' http://cues.cfans.umn.edu/old/IPM-Tactics/IPM-tactics.html.
#'
#' **Average method (`avg`)**
#'
#' > "If the maximum temperature for the day never rises above the base temperature, then
#' no development occurs, and zero degree-days accumulate (we don't calculate negative
#' degree-day values since the development of organisms does not reverse when it is
#' cold)."
#' > --- Daniel A. Herms
#'
#' **Modified Average (`modavg`)**
#'
#' > "When the daily temperature fluctuates above and below the base temperature (as it
#' does frequently in the spring), the Average Method can underestimate the number of
#' degree-days actually experienced by a plant or insect. In this situation, the Modified
#' Average Method will calculate a higher number of degree-days, and thus can be more
#' accurate for predicting pest activity than the Average Method."
#' >  --- Daniel A. Herms
#'
#' @param min (Numeric) The lowest temperature in the day.
#' @param max (Numeric) The highest temperature in the day.
#' @param base (Numeric) The threshold for counting degree-days. This could be the
#'   temperature when plants begin their spring growth or insects become noticeably
#'   active, or some other biologically-meaningful baseline.
#' @param method (String, `avg` or `modavg`) The calculation method to use (see above).
#'   Default is `modavg`.
#'
#' @return A numeric vector of the same length as min and max, with each entry containing
#'   the degree-day value for that day.
#' @export
#'
#' @examples
#' degreedays(min = c(19, 20, 20, 21), max = c(25, 24, 23, 22), base = 22, method = "modavg")
#'
#' #> [1] 1.5 1.0 0.5 0.0
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' - Daniel A. Herms <https://scholar.google.com/citations?user=yMYvlKsAAAAJ&hl=en>
#'
#' @section Source:
#' Herms, Daniel A. 2013. “Using Degree-Days and Plant Phenology to Predict Pest
#' Activity.” IPM of Midwest Landscapes: Tactics and Tools for IPM. 2013.
#' http://cues.cfans.umn.edu/old/IPM-Tactics/IPM-tactics.html.
#'
#' @md
degreedays <- function(min, max, base, method = "modavg") {
    if (method == "avg") {
        degdays <- ((min + max)/2) - base
    } else if (method == "modavg") {
        degdays <- ((max + base)/2) - base
    } else {
        stop("Method needs to be 'avg' or 'modavg'.")
    }

    # Sometimes a negative degday is generated, for some reason.
    degdays <- replace(degdays, degdays < 0, 0)

    return(degdays)
}


#' Normalise a matrix column-wise between 0 and 1
#'
#' @param mat (Numeric) A numeric matrix.
#'
#' @return A numeric matrix. The largest value in the column will be `1`, and the other
#'    values will be proportions compared to that.
#' @export
#'
#' @examples
#' normalize_colwise(matrix(1:12, ncol = 3))
#'
#' #>      [,1]  [,2]      [,3]
#' #> [1,] 0.25 0.625 0.7500000
#' #> [2,] 0.50 0.750 0.8333333
#' #> [3,] 0.75 0.875 0.9166667
#' #> [4,] 1.00 1.000 1.0000000
#'
#' @section Authors:
#' - endamaco (<https://stackoverflow.com/users/1558222/endamaco>)
#'
#' @section Source:
#' <https://stackoverflow.com/q/14282323/5578429>
#'
#' @md
normalize_colwise <- function(mat) {
    x <- sweep(mat, 2, apply(mat, 2, min))
    sweep(mat, 2, apply(mat, 2, max), "/")
}


#' Normalise a whole matrix between 0 and 1
#'
#' @param mat (Numeric) A matrix.
#' @param from_zero (Logical, default `FALSE`) If `FALSE`, the smallest value in the
#'    matrix will be set to 0. If `TRUE`, the smallest value in the matrix will be
#'    somewhere between 0 and 1.
#'
#' @return A matrix.
#' @export
#'
#' @examples
#' mat <- matrix(1:4, ncol = 2)
#'
#' #>      [,1] [,2]
#' #> [1,]    1    3
#' #> [2,]    2    4
#'
#' normalize_whole(mat, from_zero = TRUE)
#'
#' #>      [,1] [,2]
#' #> [1,] 0.25 0.75
#' #> [2,] 0.50 1.00
#'
#' normalize_whole(mat, from_zero = FALSE)
#'
#' #>           [,1]      [,2]
#' #> [1,] 0.0000000 0.6666667
#' #> [2,] 0.3333333 1.0000000
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
normalize_whole <- function(mat, from_zero = FALSE) {
    if (from_zero == TRUE) {
        baseline <- 0
    } else {
        baseline <- min(mat)
    }

    (mat - baseline) / (max(mat) - baseline)
}



#' Mirror a matrix horizontally
#'
#' @param mat (Matrix) A matrix object
#' @param ... Optional arguments that are passed to `as.data.frame()`.
#'
#' @return A version of `mat` with its columns in reversed order.
#' @export
#'
#' @examples
#' m <- matrix(1:6, ncol = 2, nrow = 3, byrow = FALSE)
#' m
#'
#' #>      [,1] [,2]
#' #> [1,]    1    4
#' #> [2,]    2    5
#' #> [3,]    3    6
#'
#' mirror_matrix(m)
#'
#' #>      V2 V1
#' #> [1,]  4  1
#' #> [2,]  5  2
#' #> [3,]  6  3
#'
mirror_matrix <- function(mat, ...) {
    return(as.matrix(rev(as.data.frame(mat, ...))))
}



#' Concatenate numbers together
#'
#' Paste a set of numbers side-by-side. Useful for manually building ID numbers from
#' fields. Missing values are replaced with 0.
#'
#' @param ... Numbers, or strings that can be coerced to numbers.
#'
#' @return A double.
#' @export
#'
#' @examples
#' concat_nums(12, "76", NA, 1.5)
#'
#' #> [1] 127601.5
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
concat_nums <- function(...) {
    string <- paste0(...)
    string <- stringr::str_replace_all(string, "NA", "0")
    return(as.double(string))
}


#' Quick percentile overview
#'
#' Break down a vector into useful percentiles. If the 25th percentile is 10.5, for
#' example, then 25 \% of the observations are < 10.5.
#'
#' @param num (Numeric) A vector.
#' @param cuts (Numeric) A vector of percentiles to calculate.
#' @param na.rm (Logical) If `TRUE`, `NA`s will be ignored.
#' @param plot (Logical) If `FALSE` (default), returns a named numeric vector of
#'    percentiles and their values. If `TRUE`, returns a scatter plot of the percentiles
#'    along X and their values along Y.
#' @param ... Extra parameters that are passed to plot() if `plot = TRUE`.
#'
#' @return If `plot = FALSE` (default), returns a named numeric vector of percentiles and
#'    their values. If `plot = TRUE`, returns a scatter plot of the percentiles along X
#'    and their values along Y.
#' @export
#'
#' @examples
#' # round(runif(20, min = 0, max = 29))
#' vec <- c(28, 23, 3, 28, 6, 5, 21, 19, 9, 17, 22, 23, 26, 9, 5, 20, 19, 24, 3, 27)
#' perc <- percentile(vec)
#'
#' perc
#'
#' #>   0%   10%   20%   25%   33%   50%   66%   75%   80%   85%   90%   95%   99%  100%
#' #> 3.00  4.80  5.80  8.25 11.16 19.50 22.54 23.25 24.40 26.15 27.10 28.00 28.00 28.00
#'
#' perc["66%"]
#'
#' #>   66%
#' #> 22.54
#'
#' # You can also just ask for one percentile:
#' percentile(vec, 0.66)
#'
#' #>   66%
#' #> 22.54
#'
#' \dontrun{
#' percentile(vec, plot = TRUE)
#' }
#'
#' # Produces a scatter plot with percentile cuts on the X axis, value on the Y axis, and
#' # the points joined by a line to show the shape of the data.
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
percentile <- function(num, cuts = c(0, 0.10, 0.20, 0.25, 0.33, 0.50, 0.66, 0.75, 0.80, 0.85, 0.90, 0.95, 0.99, 1.0), na.rm = FALSE, plot = FALSE, ...) {
    results <- stats::quantile(num, cuts, na.rm = na.rm)

    if (plot == FALSE) {
        return(results)
    } else {
        graphics::plot(results ~ cuts, xaxt = "n", xlim = c(0, 1), type = "c", lty = 2,
             xlab = "Percentile", ylab = "Value", col = "gray", ...)
        graphics::axis(1, at = cuts, labels = names(results))
        graphics::text(x = cuts, y = results, labels = format(results, digits = 1), adj = c(0.5, 0.5),
                       col = "black", font = 2, cex = 0.75)
    }
}



#' Standard error of the mean
#'
#' The standard error of the mean is how the estimated mean changes with multiple
#' measurements (i.e. how far away the mean of each sampling event or observation is from
#' the true population mean). SE drops as sample size grows because as you take more
#' mreasurements, the sampling means cluster more closely to the true mean.
#'
#' @param vec (Numeric) A vector.
#' @param na.rm (Logical) If `TRUE`, remove `NA`s from `vec`.
#'
#' @return The standard error of the mean of `vec`.
#' @export
#'
#' @examples
#' se_mean(c(1, 2, 3, 4, NA_integer_))
#' #> NA
#'
#' se_mean(c(1, 2, 3, 4, NA_integer_), na.rm = TRUE)
#' #> [1] 0.6454972
se_mean <- function(vec, na.rm = FALSE) {
    if (na.rm == TRUE) {
        vec <- vec[!is.na(vec)]
    }

    return(stats::sd(vec) / sqrt(length(vec)))
}



#' Number of unique entries in a vector
#'
#' Shorthand for `length(unique(x))`.
#'
#' @param ... (Vector) Multiple vectors will be concatenated.
#'
#' @return The number of unique values in `...`.
#' @export
#'
#' @examples
#' my_vec <- sample(month.abb, 10, replace = TRUE)
#' #> [1] "Sep" "Nov" "Nov" "Jul" "Oct" "May" "Jun" "Nov" "Apr" "Jan"
#'
#' howmany(my_vec)
#' #> 6
#'
#' howmany(letters, LETTERS)
#' #> 52
#' @md
howmany <- function(...) {
    return(
        length(unique(c(...)))
    )
}



#' Count how many times each unique element in a vector is repeated
#'
#' @param ... (Vectors) Vectors that will be concatenated together.
#' @param sort (Logical) If `TRUE`, the results will be sorted by decreasing count.
#'
#' @return A dataframe with two columns: `unique` which lists the unique value, and
#'    `count` which shows how many times that unique value appeared in `...`.
#' @export
#'
#' @examples
#' count_unique(sample(letters, size = 10, replace = TRUE))
#'
#' #>    unique  count
#' #> 1       e      1
#' #> 2       g      1
#' #> 3       i      2
#' #> 4       m      1
#' #> 5       n      1
#' #> 6       o      1
#' #> 7       p      2
#' #> 8       y      1
#'
#' @md
count_unique <- function(..., sort = FALSE) {
    vec <- c(...)
    counts <- table(vec)
    if (sort == TRUE) counts <- sort(counts, decreasing = TRUE)

    df <- as.data.frame(counts, stringsAsFactors = FALSE)
    names(df) <- c("unique", "count")

    return(df)
}



#' Check if an integer is a prime number
#'
#' @param num (Numeric) An integer.
#'
#' @return `TRUE` if `num` is prime.
#' @export
#'
#' @examples
#' is.prime(2)
#' #> [1] TRUE
#'
#' is.prime(3)
#' #> [1] TRUE
#'
#' is.prime(4)
#' #> [1] FALSE
#'
#' is.prime(5)
#' #> [1] TRUE
#'
#' @section Authors:
#' - flodel (<https://stackoverflow.com/users/1201032/flodel>)
#' - geotheory (<https://stackoverflow.com/users/1156245/geotheory>)
#' - Desi Quintans (<http://www.desiquintans.com)
#'
#' @section Source:
#' <https://stackoverflow.com/a/19767707/5578429>
#'
#' @md
is.prime <- function(num) {
    if (num == 2) {
        TRUE
    } else if (any(num %% 2:(num-1) == 0)) {
        FALSE
    } else {
        TRUE
    }
}



#' Split a vector into n groups
#'
#' @param vec (Numeric or Character) A vector. It will be sorted with `sort()` for grouping,
#'    but returned in the original order.
#' @param g (Integer) The maximum number of groups to split `vec` into (for some group
#'    sizes, the maximum will not be used).
#' @param balance (Logical) If `TRUE`, try to have equal numbers of observations per group.
#'
#' @return A vector of integers.
#' @export
#'
#' @examples
#' testvec <- c(0.7685, 0.4116, 0.1416, 0.8450, 0.9021, 0.4965, 0.8341, 0.0438)
#'
#' order(testvec)
#' #> [1] 8 3 2 6 1 7 4 5
#'
#' split_n(testvec, 4)
#' #> [1] 4 2 1 3 1 4 2 3
#'
#' split_n(testvec, 7, balance = TRUE)  # The range of groups is limited
#' #> [1] 4 2 1 3 1 4 2 3
#'
#' split_n(testvec, 7, balance = FALSE)  # Try to use the whole range
#' #> [1] 7 2 1 5 1 6 3 4
#'
#' split_n(testvec, 3)  # Sometimes unbalanced groups are inevitable
#' #> [1] 3 1 1 2 1 3 2 2
#'
#' @md
split_n <- function(vec, g, balance = TRUE) {
    n <- length(vec)

    num_repeats <- ifelse(balance == TRUE, ceiling(n/g), round(n/g))

    splits <- rep(1:g, each = num_repeats, length.out = n)

    # https://stackoverflow.com/a/1569203/5578429
    return(sort(splits)[order(vec)])
}

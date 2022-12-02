# Functions that work on numeric vectors only.


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



#' Round numbers to a nearest "pretty" value
#'
#' It's sometimes useful to round numbers to a predictable range, e.g. when preparing
#' graph axes or perhaps for filtering or presenting data. This function can round numbers
#' to the nearest, say, 0.5.
#'
#' @param num (Numeric) A vector of numbers.
#' @param to (Numeric) What interval should `num` be rounded to?
#' @param dir (Optional Char: `"up"`, `"down"`, `"both"`, or omitted) Controls the rounding
#'   function used. If omitted or `"both"`, the `round()` function is used. If `"up"`, `ceiling()` is
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

    if (is.null(dir) || dir == "both") return(round(num / to) * to)

    if (dir == "up") return(ceiling(num / to) * to)

    if (dir == "down") return(floor(num / to) * to)
}


#' Round numbers to a fixed number of decimal places
#'
#' @param x (Numeric) A vector of numbers.
#' @param digits (Numeric) Number of decimal places to keep.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' vec <- c(1.739006, 2, -1.4, 1.05, 1.90, 3.826)
#' round_to(vec, digits = 3)
#' #> [1] "1.739"  "2.000"  "-1.400" "1.050"  "1.900"  "3.826" 
#'
#' @section Authors:
#' - Jeromy Anglim (<https://stackoverflow.com/users/180892/jeromy-anglim>)
#'
#' @section Source:
#' <https://stackoverflow.com/a/12135122>
#'
#' @md
round_to <- function(x, digits = 2, ...) {
    nums_as_char <- trimws(format(round(x, digits = digits), nsmall = digits))
    as.numeric(nums_as_char)
}



#' Calculate degree-days
#'
#' "In a nutshell: heating degree days are a measure of how much (in degrees), and for
#' how long (in days), the air temperature was below a certain level." --- Martin 
#' Bromley, <http://www.degreedays.net/introduction>
#'
#' This function exposes two calculation methods that were taken from
#'
#' Herms, Daniel A. 2013. “Using Degree-Days and Plant Phenology to Predict Pest
#' Activity.” IPM of Midwest Landscapes: Tactics and Tools for IPM. 2013.
#' http://cues.cfans.umn.edu/old/IPM-Tactics/IPM-tactics.html.
#'
#' **Average method (`avg`)**
#'
#' "If the maximum temperature for the day never rises above the base temperature, then
#' no development occurs, and zero degree-days accumulate (we don't calculate negative
#' degree-day values since the development of organisms does not reverse when it is
#' cold)." --- Daniel A. Herms
#'
#' **Modified Average (`modavg`)**
#'
#' "When the daily temperature fluctuates above and below the base temperature (as it
#' does frequently in the spring), the Average Method can underestimate the number of
#' degree-days actually experienced by a plant or insect. In this situation, the Modified
#' Average Method will calculate a higher number of degree-days, and thus can be more
#' accurate for predicting pest activity than the Average Method." --- Daniel A. Herms
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

#' @rdname normalize_colwise
#' @export
normalise_colwise <- normalize_colwise



#' Normalise a whole matrix or vector between 0 and 1
#'
#' @param mat (Numeric) A matrix or a vector.
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

#' @rdname normalize_whole
#' @export
normalise_whole <- normalize_whole



#' Mirror a matrix horizontally
#'
#' @param mat (Matrix) A matrix object
#' @param MARGIN (Integer) `1` mirrors the order of rows, `2` mirrors the order
#'    of columns.
#'
#' @return A version of `mat` with its columns or rows in reversed order. Names are
#'    preserved if the matrix has any.
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
#' # Just like apply(), MARGIN = 1 is rows and MARGIN = 2 is cols.
#'
#' mirror_matrix(m, 2)
#'
#' #>      [,1] [,2]
#' #> [1,]    4    1
#' #> [2,]    5    2
#' #> [3,]    6    3
#'
#' mirror_matrix(m, 1)
#'
#' #>      [,1] [,2]
#' #> [1,]    3    6
#' #> [2,]    2    5
#' #> [3,]    1    4
#'
mirror_matrix <- function(mat, MARGIN = 2) {
    new_order <- dim(mat)[MARGIN]:1

    if (MARGIN == 2) {
        # Columns in reverse order
        mat[, new_order]
    } else if (MARGIN == 1) {
        # Rows in reverse order
        mat[new_order, ]
    } else {
        stop("Argument 'MARGIN' must be set to either '1' (rows) or '2' (cols).")
    }
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
#' example, then 25 \% of the observations are < 10.5. Gives you results as a 
#' named numeric vector, a Compact Line Display, or a percentile plot.
#'
#' @param num (Numeric) A vector.
#' @param cuts (Numeric) A vector of percentiles to calculate.
#' @param na.rm (Logical) If `TRUE`, `NA`s will be ignored.
#' @param cld (`glue::glue()` specification) A **C**ompact **L**ine **D**isplay 
#'    of your results, in `glue()` format. Access each of the cut results as 
#'    `{p1}`, `{p2}`, `{p3}`, etc. See examples for more. Incompatible 
#'    with `plot = TRUE`.
#' @param plot (Logical) If `FALSE` (default), returns a named numeric vector of
#'    percentiles and their values. If `TRUE`, returns a scatter plot of the percentiles
#'    along X and their values along Y.
#' @param ... Extra parameters that are passed to plot() if `plot = TRUE`.
#'
#' @return By default, returns a named numeric vector of percentiles and their 
#'    values. \cr
#'    If `cld` has a `Character` string in it, then returns a 
#'    glue/Character vector. \cr
#'    If `plot = TRUE`, returns a scatter plot of the percentiles along X
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
#' # You can get a _C_ompact _L_ine _D_isplay of your results. Access each element
#' # of your results as `{p1}`, `{p2}`, `{p3}`, etc.
#' percentile(vec, c(0.05, 0.5, 0.95), cld = "{p1} ({p2}) {p3}")
#' 
#' #> 3 (19.5) 28
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
percentile <- function(num, cuts = c(0, 0.025, 0.10, 0.20, 0.25, 0.33, 0.50, 0.66, 0.75, 0.80, 0.85, 0.90, 0.95, 0.99, 0.9975, 1.0), na.rm = FALSE, cld = FALSE, plot = FALSE, ...) {
    results <- stats::quantile(num, cuts, na.rm = na.rm)

    if (typeof(cld) == "character") {
        names(results) <- paste0("p", seq_along(results))
        results <- signif(results, digits = 2)
        return(glue::glue_data(results, cld))
    } else if (plot == FALSE) {
        return(results)
    } else {
        graphics::plot(results ~ cuts, xaxt = "n", type = "c", lty = 2,
             xlab = "Percentile", ylab = "Value", col = "gray", ...)
        graphics::axis(1, at = cuts, labels = names(results))
        graphics::text(x = cuts, y = results, labels = format(results, digits = 3), adj = c(0.5, 0.5),
                       col = "black", font = 2, cex = 0.75)
    }
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



#' Cumulative percentage
#'
#' @param nums (Numeric) A vector of numbers that will be used in the order they 
#'    are provided.
#'
#' @return A Numeric vector.
#' @export
#'
#' @examples
#' cumpct(rep(1, 10))
#' 
#' ## [1] 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
#' 
#' @md
cumpct <- function(nums) {
    cumsum(nums) / sum(nums)
}



# Mark outliers in a vector according to the percentile method
# https://stackoverflow.com/a/49090426/5578429
#' Mark outliers using the 1.5 * IQR method
#'
#' @param x (Numeric) Vector to analyse.
#' @param multi (Numeric) Multiplication factor for the IQR. `1.5` by default.
#'
#' @return A Numeric vector that contains elements of `x` that are outliers.
#' @export
#'
#' @examples
#' IQR_outliers(c(-3000, 1:100, 1000))
#' 
#' ## [1] -3000  1000
#'
#' @section Authors:
#' - 42- (https://stackoverflow.com/users/1855677/42)
#'
#' @section Source:
#' <https://stackoverflow.com/a/49090426/5578429>
#' 
#' @md
IQR_outliers <- function(x, multi = 1.5) {
    if(any(is.na(x)))
        stop("x is missing values")
    
    if(!is.numeric(x))
        stop("x is not numeric")
    
    Q3 <- stats::quantile(x, 0.75)
    Q1 <- stats::quantile(x, 0.25)
    IQR   <- (Q3 - Q1)
    left  <- (Q1 - (multi * IQR))
    right <- (Q3 + (multi * IQR))
    
    c(x[x < left], x[x > right])
}



#' Replace p-values with significance codes
#'
#' @param p (Numeric) A vector of p-values.
#' @param codes (Character) A vector of 6 codes to use if `p <= threshold`. The 
#'    thresholds (in order) are: 0.0001, 0.001, 0.01, 0.05, 0.10, and 1.
#'
#' @return A Character vector.
#' @export
#'
#' @examples
#' encode_signif(c(0, 0.001, 0.01, 0.05, 0.10))
#' 
#' ## [1] "****" "***"  "**"   "*"    ""
#' 
#' @md
encode_signif <- function(p, codes = c("****", "***", "**", "*", "^", "")) {
    dplyr::case_when(p <= 0.0001 ~ codes[1],
                     p <= 0.001  ~ codes[2],
                     p <= 0.01   ~ codes[3],
                     p <= 0.05   ~ codes[4],
                     p <  0.10   ~ codes[5], 
                          TRUE   ~ codes[6])
}



#' Find and mark the longest run of TRUEs in a boolean vector
#'
#' @param vec (Logical) A vector.
#'
#' @return A new logical vector of the same length as `vec`, where the longest run of
#'   TRUEs is marked with TRUE and all other values are marked FALSE. If there are two
#'   runs of TRUE with equal length in `vec`, both will be reported in the results.
#'
#' @export
#'
#' @examples
#' input <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
#' mark_longest_run(input)
#'
#' #> [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
#'
#' @section Authors:
#' - docendo discimus (<https://stackoverflow.com/users/3521006>)
#'
#' @section Source:
#' <https://stackoverflow.com/a/37447844>
#'
#' @md
mark_longest_run <- function(vec) {
    return(with(rle(vec), rep(lengths == max(lengths[values]) & values, lengths)))
}



#' Mark the location of the last maximum value in a vector
#'
#' @param vec (Numeric) A vector.
#' @param threshold (Numeric or NULL) The smallest acceptable peak value. NULL means no
#'    threshold.
#'
#' @return A bool vector of the same length as `vec`, with the last maximum marked with
#'    a TRUE.
#' @export
#'
#' @examples
#' input <- c(1, 2, 3, 3, 1)
#' mark_last_peak(input, threshold = NULL)
#'
#' #> [1] FALSE FALSE FALSE  TRUE FALSE
#'
#' mark_last_peak(input, threshold = 4)
#'
#' #> [1] FALSE FALSE FALSE FALSE FALSE
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
mark_last_peak <- function(vec, threshold = NULL) {
    maxVal <- max(vec)  # Find highest value in the vector.
    
    boolVec <- vec == maxVal  # Get the position of this value in the original vector.
    
    if (!is.null(threshold)) {
        # If the maximum number in the vector is not >= the threshold value, return a
        # vector of FALSEs.
        if (maxVal < threshold) {
            boolVec[] <- FALSE
            return(boolVec)
        }
    }
    
    # If the highest value occurs many times, only return the last one.
    lastPeak <- max(which(boolVec == TRUE))
    boolVec[] <- FALSE
    boolVec[lastPeak] <- TRUE  # Set every entry to FALSE except the last peak.
    
    # boolVec
    
    return(boolVec)
}



#' nth Triangle Number (like factorial but with addition)
#'
#' @param nums (Numeric) A vector of numbers.
#' @param coerce (Name/Symbol) The name of a function that will be used to coerce
#'    `nums` into integers, e.g. `round` or `ceiling` or `floor`.
#' @param ... (dots) Extra arguments sent to the function named in `coerce`.
#' 
#'
#' @return A Numeric vector where each element is the nth triangle number of the 
#'    original element.
#'    
#' @export
#'
#' @examples
#' triangle_num(1:10)
#' 
#' #>  [1]  1  3  6 10 15 21 28 36 45 55
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' 
#' @section Source:
#' <https://math.stackexchange.com/a/60581>
#' 
#' @md
triangle_num <- function(nums, coerce = round, ...) {
    ints <- do.call(coerce, list(nums, ...))
    
    (ints^2 + ints) / 2
}



#' Replace element with NA if it is less than the elements before and after it
#'
#' @param vec (Numeric) A vector.
#' 
#' @return A Numeric vector.
#'    
#' @export
#'
#' @examples
#' tree_height_per_month <- c(1, 2, 3, 2, 4, 7, 7, 8)
#' 
#' omit_dips(tree_height_per_month)
#' 
#' #> [1]  1  2  3 NA  4  7  7  8
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' 
#' @md
omit_dips <- function(vec) {
    # Base R lag and lead
    before <- function(vec) { c(NA, vec[seq_len(length(vec) - 1)]) }
    after <- function(vec) { c(vec[-seq_len(1)], NA) }
    
    ifelse((!is.na(before(vec) & !is.na(after(vec)))) & 
               (vec < before(vec) & vec < after(vec)), NA, vec)
}



#' Is a vector element surrounded by certain values?
#'
#' Checks if an element in a vector is flanked, i.e. if the elements before and after it
#' are `%in%` a vector of candidates.
#'
#' @param vec (Vector) A vector.
#' @param items (Vector) A vector.
#' @param edges_as_na (Logical) If `TRUE` (default), the computation will add `NA` to the 
#'     start and end of `vec` *temporarily*. If `FALSE`, it will reuse the first and last 
#'     values. See examples.
#'     
#' @return A Logical vector of the same length as `vec`.
#'    
#' @export
#'
#' @examples
#' test_vec <- c(1, NA, 2, NA, 3, 4, NA, 5)
#' 
#' is_flanked(test_vec, items = c(NA), edges_as_na = TRUE)
#' 
#' # The edges are regarded as flanked by `NA` because `edges_as_na == TRUE` adds `NA` to
#' # the start and end of `vec`.
#' #> [1]  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE
#' 
#' 
#' is_flanked(test_vec, items = c(NA), edges_as_na = FALSE)
#' 
#' #> [1] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' 
#' @md
is.flanked <- function(vec, items, edges_as_na = TRUE) {
    if (edges_as_na == TRUE) {
        lag_vec <- c(NA, vec[1:length(vec)-1])
        lead_vec <- c(vec[2:length(vec)], NA)
    } else {
        lag_vec <- c(vec[1], vec[1:length(vec)-1])
        lead_vec <- c(vec[2:length(vec)], vec[length(vec)])
    }
    
    lag_vec %in% items & vec %notin% items & lead_vec %in% items
}

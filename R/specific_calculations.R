# Calculating specific metrics, statistics, etc.


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



#' Bootstrapped confidence interval of the mean
#'
#' Quick confidence interval calculation using bootstrapping, which makes no assumptions
#' about the distribution of the data.
#'
#' By default, this is calculated using the percentile method, but it also supports the
#' Bias-Corrected and Accelerated (BCA) method (DOI: 10.1080/01621459.1987.10478410), which
#' is useful if you want to show that the lower and upper bounds of a statistic are
#' asymmetric around the point estimate.
#'
#' @param vec (Numeric) The numeric vector to calculate a CI for.
#' @param conf (Double) The confidence level, by default `0.95` (95% CI).
#' @param R (Integer) Number of bootstrap repetitions.
#' @param type (Character) Either `"perc"` (percentile method, default) or `"bca"`
#'    (bias-corrected and accelerated method).
#'
#' @return A list with three items: `ci_lwr` contains the lower bound, `ci_est` contains
#'     the point estimate, and `ci_upr` contains the upper bound. If you are using this
#'     inside a dataframe, look at `dplyr::unnest()` to split this list out into columns.
#' @export
#'
#' @examples
#' set.seed(12345)
#' nums <- c(rnorm(10), NA_real_)
#'
#' nums
#'
#' ## [1]  0.5855288  0.7094660 -0.1093033 -0.4534972  0.6058875 -1.8179560  0.6300986
#' ## [8] -0.2761841 -0.2841597 -0.9193220         NA
#'
#' boot_ci_mean(nums)
#'
#' ## $ci_lwr
#' ## [1] -0.6455236
#' ##
#' ## $ci_est
#' ## [1] -0.1329441
#' ##
#' ## $ci_upr
#' ## [1] 0.3430405
#'
#' # Demonstration of unnesting into a dataframe
#' # library(dplyr)
#' # library(tidyr)
#' #
#' # iris %>%
#' #     group_by(Species) %>%
#' #     summarise(mean_boot = boot_ci_mean(Petal.Length)) %>%
#' #     unnest(mean_boot, names_sep = ".")
#'
#' ## # A tibble: 3 × 4
#' ##   Species    mean_boot.ci_lwr mean_boot.ci_est mean_boot.ci_upr
#' ##   <fct>                 <dbl>            <dbl>            <dbl>
#' ## 1 setosa                 1.41             1.46             1.51
#' ## 2 versicolor             4.13             4.26             4.39
#' ## 3 virginica              5.40             5.55             5.7
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' @md
boot_ci_mean <- function(vec, conf = 0.95, R = 999, type = "perc") {
    if (length(type) != 1 | sum(type %in% c("bca", "perc")) != 1) {
        stop('`type` argument must be one of "bca" or "perc".')
    }
    
    boot_mean <- function(v, i) {
        mean(v[i], na.rm = TRUE)
    }
    
    boot_out <- boot::boot(data = vec, statistic = boot_mean, R = R)
    cis <- boot::boot.ci(boot_out, conf = conf, type = type)
    
    if (is.null(cis)) {
        return(data.frame(
            ci_lwr = NA_real_,
            ci_est = NA_real_,
            ci_upr = NA_real_
        ))
    }
    
    return(data.frame(
        ci_lwr = cis[[4]][4],
        ci_est = cis$t0,
        ci_upr = cis[[4]][5]
    ))
}

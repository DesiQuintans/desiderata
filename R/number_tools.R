# Functions that work on numeric vectors only.



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

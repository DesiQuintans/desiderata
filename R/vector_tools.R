# Functions that work on any vector type.


#' Mode of a vector (numeric/character/factor)
#'
#' There is no built-in function to find the mode of something. This function
#' can find the mode of a numeric, character, or factor vector. By default it
#' will return multiple values in a multi-modal dataset, but there are several
#' methods of tie-breaking included.
#'
#' If all values are unique, it will return **all** of the values unless you
#' choose to break the tie.
#'
#' @param x (Char/Numeric/Factor) A vector.
#' @param break_ties (Character) If more than one mode is found, how should the
#'    tie be broken?
#'    - `"no"` or `FALSE`: Return a vector of all of the modes that were found.
#'    - `"random"`: Randomly choose one of the modes to return.
#'    - `"mean"`: Return the average of all of the modes (for numeric vectors).
#'    - `"first"`: Return the first mode found.
#'    - `"last"`: Return the last mode found.
#'    - `"median"`: Return the median value of all of the modes.
#'    - `"median l"` or `"median r"`: Return the mode to the left or right of the median.
#'    - `"NA"`: Return NA. Useful if you only want one clear winner.
#' @param na.rm (Logical) If `TRUE`, NAs will be silently removed.
#' @param ties Deprecated (2019-02-26). Use `break_ties` instead.
#' @param mean Deprecated (2019-02-26). Use `break_ties` instead.
#'
#' @return A vector of the mode value(s).
#' @export
#'
#' @examples
#' vec <- c(1, 2, 3, 4, 4, 4, 3, 3, NA, NA, NA)
#'
#' Mode(vec, break_ties = "no")
#' #> [1]  3  4 NA
#'
#' Mode(vec, break_ties = "no", na.rm = TRUE)
#' #> [1] 3 4
#'
#' Mode(vec, break_ties = "mean", na.rm = FALSE)
#' #> [1] NA
#'
#' Mode(vec, break_ties = "mean", na.rm = TRUE)
#' #> [1] 3.5
#'
#' Mode(vec, break_ties = "median", na.rm = TRUE)
#' #> [1] 3
#'
#' Mode(letters[1:4], break_ties = "no")
#' #> [1] "a" "b" "c" "d"
#' 
#' Mode(letters[1:4], break_ties = "median l")
#' #> "b"
#' 
#' Mode(letters[1:4], break_ties = "median r")
#' #> "c"
#' 
#' Mode(letters[1:4], break_ties = "random")
#' #> [1] "a"
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
Mode <- function(x, break_ties = "no", na.rm = FALSE, ties = NULL, mean = NULL) {
    if (na.rm) {
        x = x[!is.na(x)]
    }
    
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    result <- ux[tab == max(tab)]
    
    if (is.null(mean) == FALSE) {
        # Deprecation began on 2019-02-26.
        .Deprecated(msg = "The 'mean' argument in Mode() is deprecated. Use 'break_ties' instead.")
    }
    
    if (is.null(ties) == FALSE) {
        if (mean == TRUE) {
            break_ties <- "mean"
        } else {
            if (ties == TRUE) {
                break_ties <- "first"
            } else {
                break_ties <- "no"
            }
        }
        
        msg <- uw("The 'ties' argument in Mode() is deprecated. Use 'break_ties' instead.\n
                  Based on your arguments, Mode() will default to break_ties = '", 
                  break_ties, "'.",
                  collapse = "", join = "")
        
        # Deprecation began on 2019-02-26.
        .Deprecated(msg = msg)
    }
    
    if (length(result) > 1) {
        switch(break_ties,
               "first"    = return(result[1]),
               "last"     = return(result[length(result)]),
               "random"   = return(sample(result, 1)),
               "mean"     = return(mean(result, na.rm = na.rm)),
               "NA"       = return(methods::as(NA, class(x))),
               "median"   = return(result[        stats::median(seq_along(result))]),
               "median l" = return(result[floor(  stats::median(seq_along(result)))]),
               "median r" = return(result[ceiling(stats::median(seq_along(result)))]),
               "no"       = return(result)
        )
    } else {
        return(result)
    }
}



#' Count how many times each unique element in a vector is repeated
#'
#' @param ... (Vectors) Vectors that will be concatenated together.
#' @param sort (Logical) If `TRUE`, the results will be sorted by decreasing count.
#' @param useNA (Character) Include NAs in the result? Set to `no`, `ifany`, or
#'    `always`.
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
count_unique <- function(..., sort = FALSE, useNA = "ifany") {
    vec <- c(...)
    counts <- table(vec, useNA = useNA)
    if (sort == TRUE) counts <- sort(counts, decreasing = TRUE)
    
    df <- as.data.frame(counts, stringsAsFactors = FALSE)
    names(df) <- c("unique", "count")
    
    return(df)
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



#' Inverse match
#'
#' Flags the elements of x that are not in y.
#'
#' @name notin
#' @usage x \%notin\% y
#'
#' @param x (Vector) The values to be matched. Long vectors (2^31 elements) are supported.
#' @param y (Vector) The values to be matched against. Long vectors are not supported.
#'
#' @return A logical vector of the same length as `x`, with `TRUE` if the element was
#'    found in `y`, and `FALSE` if it was not.
#' @export
#'
#' @examples
#' c(1, 4, 21, 7, -3) %in% 0:10
#' #> [1]  TRUE  TRUE FALSE  TRUE FALSE
#'
#' c(1, 4, 21, 7, -3) %notin% 0:10
#' #> [1] FALSE FALSE  TRUE FALSE  TRUE
#'
#' @section Authors:
#' - R Core Team (<https://www.r-project.org/contributors.html>)
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
"%notin%" <- function(x, y) {
    !(match(x, y, nomatch = 0) > 0)
}



#' Percentage of matching elements between two vectors
#'
#' Find the percentage of elements in x that are present in y.
#'
#' @name pctin
#' @usage x \%pctin\% y
#'
#' @param x (Vector) The values to be matched.
#' @param y (Vector) The values to be matched against.
#'
#' @return The percentage of elements in x that are present in y.
#' @export
#'
#' @examples
#' c(1, 4, 21, 7, -3) %in% 0:10
#' #> [1]  TRUE  TRUE FALSE  TRUE FALSE
#'
#' c(1, 4, 21, 7, -3) %pctin% 0:10
#' #> [1] 0.6
#'
#' @section Authors:
#' - GSee (<https://stackoverflow.com/users/967840/gsee>)
#'
#' @section Source:
#' <https://stackoverflow.com/a/13830068/5578429>
#'
#' @md
"%pctin%" <- function(x, y) {
    length(x[x %in% y])/length(x)
}



#' Keep the first n unique elements in a vector
#' 
#'
#' @param vec (Vector) Any vector.
#' @param n (Integer) The maximum number of elements to keep from `vec`. If `n` is 
#'    greater than the number of elements in `vec`, the entirety of `vec` will be
#'    returned.
#' @param sort (Character) `"ascending"` (or `"asc"` or `"a"`) sorts `vec` in 
#'    ascending order (the default order of `sort()`). `"descending"` (or `"desc"` 
#'    or `"d"`) sorts in reverse. Use `"no"` (or `"n"`) to leave the elements of 
#'    `vec` in their original order.
#'
#' @return A vector of the same type as `vec`.
#' @export
#'
#' @examples
#' set.seed(12345)
#' lets <- sample(letters[1:10], 50, replace = TRUE)
#' 
#' unique_n(lets, 10, sort = "no")
#' 
#' ## [1] "c" "j" "h" "b" "f" "g" "a" "d" "i" "e"
#' 
#' unique_n(lets, 8, sort = "asc")
#' 
#' ## [1] "a" "b" "c" "d" "e" "f" "g" "h"
#' 
#' unique_n(lets, 5, sort = "desc")
#' 
#' ## [1] "j" "i" "h" "g" "f"
#' 
#' unique_n(1:5, 20)  # Asking for 20 elements, but there are only 5 uniques.
#' 
#' ## [1] 1 2 3 4 5
#' 
#' @md
unique_n <- function(vec, n, sort = "no") {
    deduped <- unique(vec)
    len <- length(deduped)
    
    if (grepl("^(a|d)", sort, ignore.case = TRUE) == TRUE) {
        deduped <- sort(deduped,
                        decreasing = grepl("^d", sort, ignore.case = TRUE))
    }
    
    deduped[1:ifelse(n > len, len, n)]
}

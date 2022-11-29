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



#' Assign elements in a vector to groups
#'
#' In contrast to `desiderata::split_size()` which splits a vector into an 
#' arbitrary number of chunks as long as each chunk has `n` or fewer entries 
#' inside it, `desiderata::assign_groups()` splits the vector into `n` 
#' chunks, possibly with a different number of entries per chunk.
#' 
#'
#' @param vec (Numeric or Character) A vector. No sorting is done to the vector
#'    so if you want to group based on some kind of ordering, you need to do it
#'    beforehand.
#' @param g (Integer) The maximum of of groups to split `vec` into (the maximum
#'    group size may not be reached, e.g. grouping 4 elements into 6 groups).
#' @param balance (Logical) If `TRUE`, try to have equal numbers of observations 
#'    per group.
#' @param dedupe (Logical) If `TRUE`, duplicate values in `vec` will be ignored 
#'    when generating the groups, ensuring that identical values go into the 
#'    same group. This can unbalance the groups even if `balance = TRUE`.
#'
#' @return An integer vector of the same length as `vec`.
#' @export
#'
#' @examples
#' testvec <- c(4, 7, 8, 2, 2, 2, 5, 1, 6, 3)
#' 
#' # 10 values assigned to 4 balanced groups.
#' assign_groups(testvec, 4, balance = TRUE, dedupe = FALSE)
#' #> [1] 1 1 1 2 2 2 3 3 3 4
#' 
#' # 10 values assigned to 4 unbalanced groups.
#' assign_groups(testvec, 4, balance = FALSE, dedupe = FALSE)
#' #> [1] 1 1 1 1 2 2 3 3 4 4
#' 
#' # 8 values (plus 2 duplicates) assigned to 4 groups.
#' assign_groups(testvec, 4, balance = TRUE, dedupe = TRUE)
#' #> [1] 1 1 2 2 2 2 3 3 4 4
#' 
#' @md
assign_groups <- function(vec, g, balance = TRUE, dedupe = TRUE) {
    # Original data. Will be used for the output.
    input <- dplyr::tibble(vec = vec)
    
    # Working data, used for generating the groups.
    if (dedupe == TRUE) {
        work <- dplyr::distinct(input, vec)
    } else {
        work <- input
    }
    
    # Make the groups.
    n <- nrow(work)
    
    num_repeats <- ifelse(balance == TRUE, ceiling(n/g), round(n/g))
    
    # I sort the reps to avoid situations where the groups start repeating, and
    # data from later in the dataframe gets assigned to groups that occurred 
    # earlier, e.g. c(1, 1, 2, 2, 3, 3, 1, 1). This way, early data gets put into
    # early groups even if the groups are unbalanced.
    work <- dplyr::mutate(work, 
                          group = sort(rep(1:g, each = num_repeats, length.out = n)))
    
    if (dedupe == TRUE) {
        input <- dplyr::left_join(input, work, by = "vec")
    } else {
        input <- work
    }
    
    return(dplyr::pull(input, group))
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



#' Split a vector into chunks of size `n`
#'
#' In contrast to `desiderata::assign_groups()` which splits a vector into `n` 
#' chunks (possibly with a different number of entries per chunk), 
#' `desiderata::split_size()` splits a vector into an arbitrary number of chunks
#' as long as each chunk has `n` or fewer entries inside it.
#'
#' @param vec (Vector) Any vector.
#' @param size (Integer) The number of entries per chunk.
#'
#' @return A list of dynamic length, where each entry is a chunk from `vec` of 
#'     length 1 up to `size`.
#' @export
#'
#' @examples
#' split_size(letters, 5)
#' 
#' ## $`1`
#' ## [1] "a" "b" "c" "d" "e"
#' ## 
#' ## $`2`
#' ## [1] "f" "g" "h" "i" "j"
#' ## 
#' ## $`3`
#' ## [1] "k" "l" "m" "n" "o"
#' ## 
#' ## $`4`
#' ## [1] "p" "q" "r" "s" "t"
#' ## 
#' ## $`5`
#' ## [1] "u" "v" "w" "x" "y"
#' ## 
#' ## $`6`
#' ## [1] "z"
#' 
#' @section Authors:
#' - Harlan (<https://stackoverflow.com/users/135944/harlan>)
#' - dfrankow (<https://stackoverflow.com/users/34935/dfrankow>)
#'
#' @section Source:
#' <https://stackoverflow.com/a/3321659/5578429>
#' 
#' @md
split_size <- function(vec, size) {
    split(vec, ceiling(seq_along(vec) / size))
}



#' Opposite of `is.na()`
#'
#' Checks that vector elements are not `NA`. This is more readable and noticeable
#' than `!is.na(vec)`, and more compact than `is.na(vec) == FALSE`.
#'
#' @param vec (Vector) Any vector.
#'
#' @return A logical vector of the same length as `vec`, with elements either 
#' `TRUE` when the element isn't `NA`, or `FALSE` when it is `NA`.
#' 
#' @export
#'
#' @examples
#' x <- c(1, NA, 2, 3, 4)
#' 
#' is.na(x)
#' 
#' ## [1] FALSE  TRUE FALSE FALSE FALSE
#' 
#' not.na(x)
#' 
#' ## [1]  TRUE FALSE  TRUE  TRUE  TRUE
#' 
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' 
#' @md
not.na <- function(vec) {
    !is.na(vec)
}



#' Opposite of `is.nan()`
#'
#' Checks that vector elements are not `NaN`. This is more readable and noticeable
#' than `!is.nan(vec)`, and more compact than `is.nan(vec) == FALSE`.
#'
#' @param vec (Vector) Any vector.
#'
#' @return A logical vector of the same length as `vec`, with elements either 
#' `TRUE` when the element isn't `NaN`, or `FALSE` when it is `NaN`.
#' 
#' @export
#'
#' @examples
#' x <- c(1, NaN, 2, 3, 4)
#' 
#' is.nan(x)
#' 
#' ## [1] FALSE  TRUE FALSE FALSE FALSE
#' 
#' not.nan(x)
#' 
#' ## [1]  TRUE FALSE  TRUE  TRUE  TRUE
#' 
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' 
#' @md
not.nan <- function(vec) {
    !is.nan(vec)
}



#' Keep every other element of a vector, flexibly
#'
#' Use a string to keep every nth element of a vector, e.g. "k-" to keep odd 
#' elements, "--k" to keep every third element, or "k-k--" to keep every 1st 
#' and 3rd element for every 5 entries. Use `k`, `y`, or `t` to keep an element, 
#' and any other character to remove it. 
#' 
#' You can also pass other vector types into `key`, and they will be coerced into
#' a logical vector where `TRUE` keeps elements and `FALSE` or `NA` removes them.
#'
#' @param vec (Vector) Any vector.
#' @param key (Character or Other) A string that controls which elements to keep 
#'     (with `k` or `y` or `t`) and which elements to omit (any other character). 
#'     This string can be arbitrarily long; it is recycled along the length 
#'     of `vec`. If you pass a different kind of vector to `key` (e.g. a numeric
#'     vector or a logical vector), then its elements will be coerced with 
#'     `as.logical()` and `TRUE` values will keep elements, and `FALSE` or `NA` 
#'     values will remove them.
#'
#' @return A vector of the same type as `vec`, but shortened according to `key`.
#' 
#' @export
#'
#' @examples
#' 
#' # By default, keeps every odd element (1st, 3rd, etc.) using "k-".
#' keep_every(letters)
#' 
#' ## [1] "a" "c" "e" "g" "i" "k" "m" "o" "q" "s" "u" "w" "y"
#' 
#' # Keep every even element
#' keep_every(letters, "-k")
#'
#' ## [1] "b" "d" "f" "h" "j" "l" "n" "p" "r" "t" "v" "x" "z"
#' 
#' # Use k/y/t to keep an element, any other character to remove it.
#' # For every 3 elements, keep the 1st and 3rd
#' keep_every(letters, "yny")
#' 
#' ## [1] "a" "c" "d" "f" "g" "i" "j" "l" "m" "o" "p" "r" "s" "u" "v" "x" "y"
#' 
#' # Pass in a vector to use it as a coerced logical vector
#' keep_every(letters, c(1, 0, 0, 2))
#' ## [1] "a" "d" "e" "h" "i" "l" "m" "p" "q" "t" "u" "x" "y"
#' 
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' - Sven Hohenstein (https://stackoverflow.com/a/13462110)
#' 
#' @md
keep_every <- function(vec, key = "k-") {
    if (is.character(key) & length(key) == 1) {
        key_chars <- strsplit(key, split = "")[[1]]
        keep_order <- grepl("(k|y|t)", key_chars, ignore.case = TRUE)
    } else {
        keep_order <- as.logical(key)
        keep_order[is.na(keep_order)] <- FALSE
    }
    
    
    return(vec[keep_order])
}

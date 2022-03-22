# Deprecated funs

#' Split a vector into n groups
#' 
#' This function was deprecated on 2022-03-22. Use `assign_groups()` instead.
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
    warning("split_n() is deprecated since 2022-03-22. Use 'assign_groups()' instead.")
    
    n <- length(vec)
    
    num_repeats <- ifelse(balance == TRUE, ceiling(n/g), round(n/g))
    
    splits <- rep(1:g, each = num_repeats, length.out = n)
    
    # https://stackoverflow.com/a/1569203/5578429
    return(sort(splits)[order(vec)])
}

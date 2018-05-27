# Extra functions that help me work inside dataframes.


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

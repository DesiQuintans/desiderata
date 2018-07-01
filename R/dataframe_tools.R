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



#' Replace all matching values in a dataframe with something else
#'
#' Uses regex to match and replace cell values. This function is intended to be used just
#' before printing a table to a Rmarkdown document; it's often useful to blank out NAs or
#' other values to minimise visual clutter. For actual data-tidying applications, it's
#' safer to use `dplyr::recode()` or `dplyr::recode_factor()`.
#'
#' By default, this function will replace cells consisting of NAs, spaces, empty strings,
#' dashes, and underscores with an empty string.
#'
#' @param df (Dataframe) A dataframe.
#' @param find (Character) A regex search pattern.
#' @param replace (Character) The string used to overwrite the matching cells.
#' @param replace_na (Logical) If `TRUE`, also overwrite R's built-in `NA` values.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' test_df <-
#'     data.frame(stringsAsFactors = FALSE,
#'                name = c("insect1", "insect2", "insect3", "insect4", "insect5",
#'                         "insect6", "insect7", "insect8", "insect9", "insect10"),
#'                family = c("Belidae", "Belidae", " ", "Coccinelidae", NA, "Coccinelidae",
#'                           "Braconidae", "_", "-", "Curculionidae"),
#'                is_cool = c("TRUE", "TRUE", NA, "TRUE", "", "TRUE", "TRUE", "-", "_",
#'                            "TRUE")
#'     )
#'
#' test_df
#' overwrite_df(test_df)
#' @md
overwrite_df <- function(df, find = "^(NA||\\s+|0|-+|_+)$", replace = "", replace_na = TRUE) {
    df_l <- df

    if (replace_na == TRUE) {
        df_l[is.na(df_l)] <- replace  # gsub can't look for R's NA values, so replace them.
    }

    out <- data.frame(lapply(df_l, function(x) { gsub(find, replace, as.character(x)) }),
                      stringsAsFactors = FALSE,
                      check.rows = FALSE,
                      check.names = FALSE,
                      fix.empty.names = FALSE)

    return(out)
}


#' Drop 'empty' columns in a dataframe
#'
#' A column is empty when every single row is NA, NULL, "", or 0.
#'
#' @param df (Dataframe) A dataframe to filter.
#'
#' @return A copy of `df` with all empty columns removed.
#' @export
#'
#' @examples
#' data <- data.frame(a = c(1, 2, 3),
#'                    b = c(0, 0, 0),
#'                    c = c(1, 1, 0),
#'                    d = c("", "", ""),
#'                    e = c("moo", "baa", "woof"))
#'
#' #> a b c d    e
#' #> 1 0 1    moo
#' #> 2 0 1    baa
#' #> 3 0 0    woof
#'
#' drop_empty_cols(data)
#'
#' #> a c    e
#' #> 1 1  moo
#' #> 2 1  baa
#' #> 3 0 woof
#'
#' @md
drop_empty_cols <- function(df) {
    base::Filter(function(x) !all(is.na(x) || is.null(x) || x == "" || x == 0), df)
}

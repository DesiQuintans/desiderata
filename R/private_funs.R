# Private functions for desiderata

#' Subset dataframe columns
#'
#' This function mostly exists so that I don't have to keep typing `df[2:length(df)]` or
#' some other variant of it. It lets me subset columns based on contiguous or
#' non-contiguous ranges.
#'
#' @param df (Dataframe) A dataframe.
#' @param from,to (Numeric or `NULL`) The start and end of a continuous range of columns
#'     that will be considered for the empty/not-empty decision. For example, columns that
#'     are always filled should be omitted (see examples). If `to` is `NULL`, it defaults
#'     to the last column in `df` so that `from = 2, to = NULL` is the same as
#'     `2:length(df)`.
#' @param cols (Numeric or `NULL`) A numeric vector of the columns to consider. This
#'     allows you to select non-contiguous columns. If the `cols` argument is being used
#'     (not-`NULL`), `from` and `to` will be ignored.
#'
#' @return A numeric vector that can be used to reference columns in `df`.
#' @md
construct_cols <- function(df, from = 1, to = NULL, cols = NULL) {
    if (is.null(from) & is.null(cols)) {
        stop("One of either the 'from' or 'cols' arguments needs to be set.")
    }

    # I briefly considered dumping 'cols' and making 'from' behave different if it had 1
    # element (from:to) versus when it had more than one element (just pass it forward,
    # assuming that the user had provided a column range). However, I realised that this
    # gave the user no way to select just one column.

    if (is.null(cols)) {
        # Construct a column reference using the 'from' and 'to' args.
        if (is.null(to)) {
            to <- length(df)
        }

        cols <- from:to
    }

    return(cols)
}
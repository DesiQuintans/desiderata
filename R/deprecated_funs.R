# Deprecated funs

#' Count the number of unique values per column in a dataframe
#'
#' Deprecated. Use `howmany(df)` instead.
#'
#' @md
howmany_df <- function(df, ...) {
    .Deprecated(
        "howmany",
        package = "desiderata",
        msg = "howmany_df() is replaced by the generic function howmany().\n")

    howmany(df, ...)
}

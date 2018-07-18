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
#' Uses regex to match and replace cell values. This function is meant for formatting
#' printed data; it's often useful to blank out NAs or other values to minimise visual
#' clutter when you are reading a table. For actual data-tidying applications, it's
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
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
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
#' Deletes columns from a dataframe if they are 'empty'. A column is empty when every
#' single row is `NA`, `NULL`, `""`, or `0`. This function is meant for formatting
#' printed data; it's often useful to omit uninformative columns to reduce visual
#' clutter when you are reading a table. Be careful when using this function to clean or
#' manipulate data because zeroes are often informative.
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
#' @param regex (Character) A regex pattern that matches a value that should be considered
#'     'empty'.
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
#' drop_empty_cols(data, regex = "moo|baa|woof")
#'
#' #> a c
#' #> 1 1
#' #> 2 1
#' #> 3 0
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
drop_empty_cols <- function(df, from = 1, to = NULL, cols = NULL, regex = "^$") {
    selected <- construct_cols(df, from = from, to = to, cols = cols)
    sub_df <- df[selected]

    base::Filter(function(x) !all(is.na(x) | is.null(x) | x == "" | x == 0 | grepl(regex, x)), sub_df)
}


#' Drop 'empty' rows in a dataframe
#'
#' Deletes rows from a dataframe if they are 'empty'. A row is empty when every single
#' cell is `NA`, `NULL`, `""`, or `0`. This function is meant for formatting printed data;
#' it's often useful to omit uninformative rows to reduce visual clutter when you are
#' reading a table. Be careful when using this function to clean or manipulate data
#' because zeroes are often informative.
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
#' @param regex (Character) A regex pattern that matches a value that should be considered
#'     'empty'.
#'
#' @return A copy of `df` with all empty rows removed, based on whether the rows in the
#'     selected columns were empty.
#' @export
#'
#' @examples
#' data <- data.frame(name = c("Jim", "Jane", "Janice", "Joe", "Jay"),
#'                    a = c(0, 0, 1, NA, 0),
#'                    b = c(1, "", 1, NA, 0),
#'                    c = c(1, 0, 2, 0, 0),
#'                    d = c(0, 0, 4, 0, 0),
#'                    e = c(0, 0, 5, 0, 0),
#'                    f = c(3, 0, 0, 0, 3),
#'                    stringsAsFactors = FALSE)
#'
#' data
#'
#' #>           1  2    3 4 5 6 7
#' #>
#' #>        name  a    b c d e f
#' #> 1       Jim  0    1 1 0 0 3
#' #> 2      Jane  0      0 0 0 0
#' #> 3    Janice  1    1 2 4 5 0
#' #> 4       Joe NA <NA> 0 0 0 0
#' #> 5       Jay  0    0 0 0 0 3
#'
#' drop_empty_rows(data)
#'
#' # Returns the whole dataframe because column 1 ('name') is never empty.
#' #>        name  a    b c d e f
#' #> 1       Jim  0    1 1 0 0 3
#' #> 2      Jane  0      0 0 0 0
#' #> 3    Janice  1    1 2 4 5 0
#' #> 4       Joe NA <NA> 0 0 0 0
#' #> 5       Jay  0    0 0 0 0 3
#'
#' drop_empty_rows(data, from = 2)
#'
#' # We get the desired result when 'name' is omitted.
#' #>        name  a  b c d e f
#' #> 1       Jim  0  1 1 0 0 3
#' #> 3    Janice  1  1 2 4 5 0
#' #> 5       Jay  0  0 0 0 0 3
#'
#' drop_empty_rows(data, regex = "^J.*?$")
#'
#' # Regex can be used to match cells that should be 'empty'.
#' #>        name  a  b c d e f
#' #> 1       Jim  0  1 1 0 0 3
#' #> 3    Janice  1  1 2 4 5 0
#' #> 5       Jay  0  0 0 0 0 3
#'
#' drop_empty_rows(data, cols = c(2, 5, 6))
#'
#' # Non-contiguous columns can be selected with 'cols'.
#' #>        name  a  b c d e f
#' #> 3    Janice  1  1 2 4 5 0
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
drop_empty_rows <- function(df, from = 1, to = NULL, cols = NULL, regex = "^$") {
    selected <- construct_cols(df, from = from, to = to, cols = cols)
    sub_df <- df[selected]

    # trimws() MUST be kept in the anonymous function below.
    # https://stackoverflow.com/a/15618761/5578429
    # When apply() is given a dataframe, it coerces it to a matrix with as.matrix(). The
    # coercion is done using format(), which pads numbers with spaces to the length
    # of the longest string in the column. This means that a df might be coerced as:
    #
    # "NA" "1" "1"
    # " 0" "0" " "    This row is wrongly kept because " 0" and " " are not 'empty'.
    # " 1" "1" "2"
    # " 1" "1" "3"
    is_empty <- apply(sub_df, MARGIN = 1,
                      function(x) {
                          y <- trimws(x, which = "both");
                          all(nchar(y) == 0 |
                              y == "" |
                              y == 0 |
                              is.na(y) |
                              is.null(y) |
                              grepl(regex, y))
                          }
                      )

    return(df[!is_empty,])
}


#' Collapse a dataframe into a vector
#'
#' Useful for taking every number in a table and plotting it in a histogram, for example.
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
#' @return A vector containing the cell contents from the selected columns of `df`.
#'     If all of the cells are numeric, the vector is Numeric. If any of the cells contain
#'     strings, the vector is Character. The columns are concatenated in order.
#' @export
#'
#' @examples
#' collapse_df(iris, cols = 1:4)
#'
#' #> [1] 5.1 4.9 4.7 4.6 5.0 5.4 4.6 5.0 4.4 4.9 5.4 4.8 ...
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
collapse_df <- function(df, from = 1, to = NULL, cols = NULL) {
    selected <- construct_cols(df, from = from, to = to, cols = cols)
    sub_df <- df[selected]

    # I wondered for a second why I should even make this a function instead of just using
    # unlist() directly. But then I realised that I would have to keep typing
    # longdataframename[2:length(longdataframename)], and that's pretty annoying.

    return(unlist(sub_df, use.names = FALSE))
}

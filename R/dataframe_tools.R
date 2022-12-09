# Extra functions that help me work inside dataframes.


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
#' single row is `NA`, `NULL`, `""`, or matches a regular expression.
#'
#' @param df (Dataframe) A dataframe.
#' @param from,to (Numeric or `NULL`) The start and end of a continuous range of columns
#'     that will be subsetted from `df`. If `to` is `NULL`, it defaults to the last
#'     column in `df` so that `from = 2, to = NULL` is the same as `2:length(df)`.
#' @param cols (Numeric or `NULL`) A numeric vector of the columns to consider. This
#'     allows you to select non-contiguous columns. If the `cols` argument is being used
#'     (not-`NULL`), `from` and `to` will be ignored.
#' @param regex (Character) A regex pattern that matches a value that should be considered
#'     'empty'.
#' @param report (Logical) If `TRUE`, print a Message with the names of the empty columns
#'     that were dropped.
#'
#' @return A subset of `df` with all empty columns removed.
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
#' #> a b c   e
#' #> 1 0 1 moo
#' #> 2 0 1 baa
#' #> 3 0 0 woof
#'
#' drop_empty_cols(data, regex = "moo|baa|woof")
#'
#' #> a b c
#' #> 1 0 1
#' #> 2 0 1
#' #> 3 0 0
#' 
#' drop_empty_cols(data, regex = "moo|baa|woof", report = TRUE)
#' 
#' #> Empty cols dropped: d, e
#' #> a b c
#' #> 1 0 1
#' #> 2 0 1
#' #> 3 0 0
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
drop_empty_cols <- function(df, from = 1, to = NULL, cols = NULL, regex = "^$",
                            report = FALSE) {
    selected <- construct_cols(df, from = from, to = to, cols = cols)
    sub_df <- df[selected]

    out <- base::Filter(function(x) !all(is.na(x) | is.null(x) | x == "" | grepl(regex, x)), sub_df)

    if (report == TRUE) {
        message("Dropped empty cols: ", fold(diff_cols(df, out), n = Inf))
    }
    
    return(dplyr::select(df, any_of(colnames(out))))
}


#' Drop 'empty' rows in a dataframe
#'
#' Deletes rows from a dataframe if they are 'empty'. A row is empty when every single
#' cell is `NA`, `NULL`, `""`, or matches a regular expression.
#'
#' @param df (Dataframe) A dataframe.
#' @param from,to (Numeric or `NULL`) The start and end of a continuous range of columns
#'     that will be subsetted from `df`. For example, columns that are always filled
#'     should be omitted (see examples). If `to` is `NULL`, it defaults to the last
#'     column in `df` so that `from = 2, to = NULL` is the same as `2:length(df)`.
#' @param cols (Numeric or `NULL`) A numeric vector of the columns to consider. This
#'     allows you to select non-contiguous columns. If the `cols` argument is being used
#'     (not-`NULL`), `from` and `to` will be ignored.
#' @param regex (Character) A regex pattern that matches a value that should be considered
#'     'empty'.
#' @param report (Logical) If `TRUE`, print a Message with the number of empty rows
#'     that were dropped.
#'
#' @return A subset of `df` with all empty rows removed.
#' @export
#'
#' @examples
#' data <- data.frame(name = c("Jim", "Jane", "Janice", "Joe", "Jay"),
#'                    a = c(0, "", 1, NA, 0),
#'                    b = c(1, "", 1, NA, 0),
#'                    c = c(1, NA, 2, 0, 0),
#'                    d = c(0, NA, 4, 0, 0),
#'                    e = c(0, "", 5, 0, 0),
#'                    f = c(3, "", 0, 0, 0),
#'                    stringsAsFactors = FALSE)
#'                    
#' data
#'
#' #>     name    a    b  c d e f
#' #> 1    Jim    0    1  1  0 0 3
#' #> 2   Jane           NA NA    
#' #> 3 Janice    1    1  2  4 5 0
#' #> 4    Joe <NA> <NA>  0  0 0 0
#' #> 5    Jay    0    0  0  0 0 0
#'
#' drop_empty_rows(data)
#'
#' # Returns the whole dataframe because column 1 ('name') is never empty.
#' #>     name    a    b  c  d e f
#' #> 1    Jim    0    1  1  0 0 3
#' #> 2   Jane           NA NA    
#' #> 3 Janice    1    1  2  4 5 0
#' #> 4    Joe <NA> <NA>  0  0 0 0
#' #> 5    Jay    0    0  0  0 0 0
#'
#' drop_empty_rows(data, from = 2)
#'
#' # We get the desired result when 'name' is omitted.
#' #>     name    a    b c d e f
#' #> 1    Jim    0    1 1 0 0 3
#' #> 3 Janice    1    1 2 4 5 0
#' #> 4    Joe <NA> <NA> 0 0 0 0
#' #> 5    Jay    0    0 0 0 0 0
#'
#' drop_empty_rows(data, from = 2, regex = "^0$")
#'
#' # Regex can be used to match cells that should be 'empty'.
#' #>     name a b c d e f
#' #> 1    Jim 0 1 1 0 0 3
#' #> 3 Janice 1 1 2 4 5 0
#'
#' drop_empty_rows(data, cols = c(2, 6))
#'
#' # Non-contiguous columns can be selected with 'cols'.
#' #>     name    a    b c d e f
#' #> 1    Jim    0    1 1 0 0 3
#' #> 3 Janice    1    1 2 4 5 0
#' #> 4    Joe <NA> <NA> 0 0 0 0
#' #> 5    Jay    0    0 0 0 0 0
#' 
#' drop_empty_rows(data, cols = c(2, 6), report = TRUE)
#'
#' #> Dropped rows: 1 in total
#' #>     name    a    b c d e f
#' #> 1    Jim    0    1 1 0 0 3
#' #> 3 Janice    1    1 2 4 5 0
#' #> 4    Joe <NA> <NA> 0 0 0 0
#' #> 5    Jay    0    0 0 0 0 0
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
drop_empty_rows <- function(df, from = 1, to = NULL, cols = NULL, regex = "^$",
                            report = FALSE) {
    selected <- construct_cols(df, from = from, to = to, cols = cols)
    sub_df <- df[selected]

    # https://stackoverflow.com/a/15618761/5578429
    # trimws() MUST be kept in the anonymous function below. This is because of how,
    # when apply() is given a dataframe, it coerces it to a matrix with as.matrix(). The
    # coercion is done using format(), which pads numbers with spaces to the length
    # of the longest string in the column. This means that a df might be coerced as:
    #
    # "NA" "1" "1"
    # " 0" "0" " "    This row is wrongly kept because " " is not 'empty'.
    # " 1" "1" "2"
    # " 1" "1" "3"
    not_empty <- apply(sub_df, 
                      MARGIN = 1,  # Along each row...
                      function(x) {
                          # Remove whitespace from each cell
                          y <- trimws(x, which = "both");
                          
                          # Negated so that non-empty rows are TRUE, and will be kept.
                          !all(nchar(y) == 0 |
                              y == "" |
                              is.na(y) |
                              is.null(y) |
                              grepl(regex, y))  
                          }
                      )

    # This is dplyr::slice() and not a simpler subset (df[!is_empty,]) because of a strange
    # quirk where dataframes with labelled variables would lose their labels if they were
    # subset in base R. dplyr::slice() keeps them.
    out <- dplyr::slice(df, which(not_empty))
    
    if (report == TRUE) {
        dropped_row_nums <- paste0(": ", fold(which(!not_empty), n = 50))
        message("Dropped ", nrow(df) - nrow(out), " empty rows", dropped_row_nums)
    }
    
    return(out)
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


#' Sort columns in a dataframe
#'
#' Sorts the columns of a dataframe, and then allows you to pull columns to the start of
#' the dataframe by name.
#'
#' @param df (Dataframe) A dataframe.
#' @param ... (Column names) If you want to manually position columns _after_ they are
#'    sorted, provide unquoted column names here. The columns in `...` will be placed
#'    first in the dataframe, and then all other unlisted columns will be placed after.
#' @param decreasing (Logical) If `FALSE`, sort columns from A-Z and 0-9. If `TRUE`, sort
#'    in reverse.
#'
#' @return A copy of `df` with reordered columns.
#' @export
#'
#' @examples
#' colnames(iris)
#'
#' #> [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"
#'
#' sorted <- sort_cols(iris)
#' colnames(sorted)
#'
#' #> [1] "Petal.Length" "Petal.Width"  "Sepal.Length" "Sepal.Width"  "Species"
#'
#' reverse <- sort_cols(iris, decreasing = TRUE)
#' colnames(reverse)
#'
#' #> [1] "Species"  "Sepal.Width"  "Sepal.Length" "Petal.Width"  "Petal.Length"
#'
#' manual <- sort_cols(iris, Species)
#' colnames(manual)
#'
#' #> [1] "Species" " Petal.Length" "Petal.Width"  "Sepal.Length" "Sepal.Width"
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
sort_cols <- function(df, ..., decreasing = FALSE) {
    dots <- dplyr::quos(...)
    sorted <- df[, order(colnames(df), decreasing = decreasing)]  # Order cols.
    return(dplyr::select(sorted, !!! dots, dplyr::everything()))
}



#' Drop invariant columns from a dataframe
#'
#' Deletes columns from a dataframe if they do not vary. For `character` and `factor`
#' columns, this means that every row of the column contains exactly the same string.
#' For `numeric` columns, the numbers are rounded to a nearest common value and then
#' checked to see if every rounded number is the same.
#'
#' @param df (Dataframe) A dataframe.
#' @param from,to (Numeric or `NULL`) The start and end of a continuous range of columns
#'     that will be used. If `to` is `NULL`, it defaults to the last column in `df` so
#'     that `from = 2, to = NULL` is the same as `2:length(df)`.
#' @param cols (Numeric or `NULL`) A numeric vector of the columns to consider. This
#'     allows you to select non-contiguous columns. If the `cols` argument is being used
#'     (not-`NULL`), `from` and `to` will be ignored.
#' @param nearest (Numeric or `NULL`) For numeric columns, this is the common value that
#'     all numbers will be rounded to. The default `NULL` uses the `mean()` of each
#'     column as the rounding target.
#' @param dir (Character or `NULL`) Controls the rounding function used. Leave as `NULL`
#'     to round up and down. Use `"up"` to round up only. Use `"down"` to round down only.
#'
#' @return A copy of `df` with all invariant columns removed.
#' @export
#'
#' @examples
#' df <- data.frame(stringsAsFactors=FALSE,
#'          char_invar = c("A", "A", "A", "A", "A"),
#'            char_var = c("A", "A", "A", "B", "A"),
#'           num_invar = c(1L, 1L, 1L, 1L, 1L),
#'          num_mean_0 = c(0, -0.1, 0.1, 0.01, -0.01),
#'             num_var = c(0, 0.2, 0.8, 0.03, 0.4)
#'       )
#'
#' df
#'
#' #>   char_invar char_var num_invar num_mean_0 num_var
#' #> 1          A        A         1       0.00    0.00
#' #> 2          A        A         1      -0.10    0.20
#' #> 3          A        A         1       0.10    0.80
#' #> 4          A        B         1       0.01    0.03
#' #> 5          A        A         1      -0.01    0.40
#'
#'
#' drop_invar_cols(df)
#'
#' #>   char_var num_var
#' #> 1        A    0.00
#' #> 2        A    0.20
#' #> 3        A    0.80
#' #> 4        B    0.03
#' #> 5        A    0.40
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
drop_invar_cols <- function(df, from = 1, to = NULL, cols = NULL,
                            nearest = NULL, dir = NULL) {
    selected <- construct_cols(df, from = from, to = to, cols = cols)
    sub_df <- df[selected]

    base::Filter(
        function(x) {
            if (is.numeric(x)) {
                # Use fuzzy (rounded) matching
                if (is.null(nearest)) nearest = mean(x, na.rm = TRUE)

                rounded <- round_to_nearest(x, to = nearest, dir = dir)
                if (howmany(rounded) == 1) return(FALSE)
            } else {
                # Use exact matching
                if (howmany(x) == 1) return(FALSE)
            }

            return(TRUE)
        }, sub_df)
}



#' First and last rows of a dataframe
#'
#' @param df (Dataframe) A dataframe.
#' @param top (Integer) The number of rows to get from the start of `df`.
#' @param tail (Integer) The number of rows to get from the end of `df`.
#'
#' @details `0` can be provided for the top and tail, in which case it will behave like 
#'    `head()` and `tail()` respectively.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # Returns 6 rows by default, just like head() does.
#' top_tail(iris)
#' 
#' #>     Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
#' #> 1            5.1         3.5          1.4         0.2    setosa
#' #> 2            4.9         3.0          1.4         0.2    setosa
#' #> 3            4.7         3.2          1.3         0.2    setosa
#' #> 148          6.5         3.0          5.2         2.0 virginica
#' #> 149          6.2         3.4          5.4         2.3 virginica
#' #> 150          5.9         3.0          5.1         1.8 virginica
#' 
#' top_tail(iris, top = 1, tail = 2)
#'
#' #>     Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
#' #> 1            5.1         3.5          1.4         0.2    setosa
#' #> 149          6.2         3.4          5.4         2.3 virginica
#' #> 150          5.9         3.0          5.1         1.8 virginica
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' 
#' @md
top_tail <- function(df, top = 3, tail = 3) {
    rows <- nrow(df)
    if (top <= 0)  top_range <- 0  else top_range  <- 1:top
    if (tail <= 0) tail_range <- 0 else tail_range <- (rows - (tail - 1)):rows
    
    df[unique(c(top_range, tail_range)), ]
}



#' Add a 'group size' column to a dataframe
#'
#' @param df (Dataframe) The dataframe.
#' @param ... (Names) Bare names of the columns of `df` that will for the groups for
#'   `dplyr::group_by()`.
#' @param .id (Character) The name of the new column. If `NA` (default), the name
#'   will be generated from the columns in `...`.
#' @param na.rm (Logical or Character) If `TRUE`, runs `tidyr::drop_na(df)` before
#'   grouping. If a `Character` vector that has column names, runs
#'   `tidyr::drop_na(df, ...)` where `...` is the column names that will be
#'   considered for dropping.
#'
#' @return An ungrouped dataframe `df` with a new column containing the group size,
#'   duplicated at each row. By default, the new column's name is generated from the
#'   groups in `...` (see examples).
#' @export
#'
#' @examples
#' 
#' sw_subset <- dplyr::select(dplyr::starwars, -(films:starships))
#' 
#' test <- add_group_size(sw_subset, species, homeworld, 
#'                        .id = "my_colname", na.rm = FALSE)
#' dplyr::glimpse(test)
#'
#' #> Observations: 87
#' #> Variables: 11
#' #> $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Le...
#' #> $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 1...
#' #> $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0...
#' #> $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brow...
#' #> $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "ligh...
#' #> $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "b...
#' #> $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 5...
#' #> $ gender     <chr> "male", NA, NA, "male", "female", "male", "female", NA...
#' #> $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan...
#' #> $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", ...
#' #> $ my_colname <int> 8, 2, 1, 8, 3, 8, 8, 2, 8, 1, 8, 1, 2, 2, 1, 1, 2, 1, ...
#'
#' test2 <- add_group_size(sw_subset, eye_color, homeworld, 
#'                         na.rm = c("hair_color", "gender"))
#'
#' # Note the automatic column names and the dropped NA rows.
#' dplyr::glimpse(test2)
#'
#' #> Observations: 82
#' #> Variables: 11
#' #> $ name                        <chr> "Luke Skywalker", "Darth Vader", "Lei...
#' #> $ height                      <int> 172, 202, 150, 178, 165, 183, 182, 18...
#' #> $ mass                        <dbl> 77.0, 136.0, 49.0, 120.0, 75.0, 84.0,...
#' #> $ hair_color                  <chr> "blond", "none", "brown", "brown, gre...
#' #> $ skin_color                  <chr> "fair", "white", "light", "light", "l...
#' #> $ eye_color                   <chr> "blue", "yellow", "brown", "blue", "b...
#' #> $ birth_year                  <dbl> 19.0, 41.9, 19.0, 52.0, 47.0, 24.0, 5...
#' #> $ gender                      <chr> "male", "male", "female", "male", "fe...
#' #> $ homeworld                   <chr> "Tatooine", "Tatooine", "Alderaan", "...
#' #> $ species                     <chr> "Human", "Human", "Human", "Human", "...
#' #> $ grpsize_eye_color_homeworld <int> 5, 1, 3, 5, 5, 2, 1, 5, 1, 2, 1, 1, 1...
#'
#'
#' @section Authors: 
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @importFrom rlang :=
#' @md
add_group_size <- function(df, ..., .id = NA, na.rm = FALSE) {
    
    if (is.na(.id)) {
        colname <- paste0("grpsize_", dots_char(..., collapse = "_"))
    } else {
        colname <- as.character(.id)
    }
    
    if (typeof(na.rm) == "character") {
        if (length(na.rm) == 0) {
            stop("Argument 'na.rm' must be TRUE, FALSE, or a character vector of
                 column names.")
        }

        df <- do.call(tidyr::drop_na, list(data = df, na.rm))
    } else if (na.rm == TRUE) {
        df <- tidyr::drop_na(df)
    }

    res <- dplyr::group_by(df, ...)
    res <- dplyr::mutate(res, !!colname := dplyr::n())
    res <- dplyr::ungroup(res)

    return(res)
}



#' Only keep rows that contain NA
#' 
#' It is sometimes useful to look at rows of a dataframe where a value is 
#' missing. For example, you may want to see why a function returns NA in some 
#' rows and not others.
#'
#' @param df (Dataframe) The dataframe
#'
#' @return A dataframe with the same number of columns as `df`, but the only
#'     rows it has are rows that have at least 1 `NA` value.
#' @export
#'
#' @examples
#' 
#' na_starwars <- rows_with_na(dplyr::starwars)
#' dplyr::glimpse(na_starwars)
#' 
#' #> Observations: 58
#' #> Variables: 13
#' #> $ name       <chr> "C-3PO", "R2-D2", "R5-D4", "Wilhuff Tarkin", "Greedo",...
#' #> $ height     <int> 167, 96, 97, 180, 173, 175, 180, 66, 200, 150, NA, 160...
#' #> $ mass       <dbl> 75, 32, 32, NA, 74, 1358, 110, 17, 140, NA, NA, 68, 89...
#' #> $ hair_color <chr> NA, NA, NA, "auburn, grey", NA, NA, "brown", "white", ...
#' #> $ skin_color <chr> "gold", "white, blue", "white, red", "fair", "green", ...
#' #> $ eye_color  <chr> "yellow", "red", "red", "blue", "black", "orange", "bl...
#' #> $ birth_year <dbl> 112, 33, NA, 64, 44, 600, NA, 896, 15, 48, NA, NA, 92,...
#' #> $ gender     <chr> NA, NA, NA, "male", "male", "hermaphrodite", "male", "...
#' #> $ homeworld  <chr> "Tatooine", "Naboo", "Tatooine", "Eriadu", "Rodia", "N...
#' #> $ species    <chr> "Droid", "Droid", "Droid", "Human", "Rodian", "Hutt", ...
#' #> $ films      <list> [<"Attack of the Clones", "The Phantom Menace", "Reve...
#' #> $ vehicles   <list> [<>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, <>, "Tri...
#' #> $ starships  <list> [<>, <>, <>, <>, <>, <>, "X-wing", <>, <>, <>, "A-win...
#' 
#' @section Authors: 
#' - Desi Quintans (<http://www.desiquintans.com>)
#' @md
#' @importFrom magrittr %>%
rows_with_na <- function(df) {
    na_count <- 
        df %>% 
        dplyr::mutate_all(~ if_na(., 1, 0)) %>% 
        rowSums()
    
    df %>% 
        dplyr::mutate(na_count_per_row = na_count) %>% 
        dplyr::filter(na_count_per_row > 0) %>% 
        dplyr::select(-na_count_per_row)
}



#' Given two dataframes, which columns appear in both of them?
#'
#' The order of `l` and `r` doesn't matter for `same_cols()`, but
#' it does for `diff_cols()`.
#'
#' @param l (Dataframe) A dataframe whose column names to compare.
#' @param r (Dataframe) A dataframe whose column names to compare.
#'
#' @return A Character vector with the names of the columns that appear
#'     in both `l` and `r`. 
#'     
#' @export
#'
#' @examples
#' iris1 <- iris[, 1:3]
#' colnames(iris1)
#' 
#' #> [1] "Sepal.Length" "Sepal.Width"  "Petal.Length"
#'
#' iris2 <- iris[, 2:5]
#' colnames(iris2)
#' #> [1]                "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"
#'
#'
#' same_cols(iris1, iris2)
#' #> [1] "Sepal.Width"  "Petal.Length"
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' @md
same_cols <- function(l, r) {
    base::intersect(colnames(l), colnames(r))
}



#' Given two dataframes, which columns are present in one but not in the other?
#' 
#' Unlike `same_cols()`, the order of `l` and `r` *does* matter for `diff_cols()`.
#'
#' @param l (Dataframe) A dataframe whose column names to compare.
#' @param r (Dataframe) A dataframe whose column names to compare.
#' @param side (Character) `"both"` or `"b"` (default) finds columns that are missing from
#'     both dataframes. `"left"` or `"l"` finds cols in `l` that are not in `r`.
#'     `"right"` or `"r"` finds cols in `r` that are not in `l`.
#'
#' @return A Character vector with the names of missing columns.
#' @export
#'
#' @examples
#' iris1 <- iris[, 1:3]
#' colnames(iris1)
#' ## [1] "Sepal.Length" "Sepal.Width"  "Petal.Length"
#'
#' iris2 <- iris[, 2:5]
#' colnames(iris2)
#' ## [1]                "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"
#'
#' diff_cols(iris1, iris2)
#' #> [1] "Sepal.Length" "Petal.Width"  "Species"
#'
#' diff_cols(iris1, iris2, side = "l")
#' #> [1] "Sepal.Length"
#'
#' diff_cols(iris1, iris2, side = "r")
#' #> [1] "Petal.Width"  "Species"
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' @md
diff_cols <- function(l, r, side = "both") {
    # Both directions need to be compared.
    set1 <- base::setdiff(colnames(l), colnames(r))
    set2 <- base::setdiff(colnames(r), colnames(l))
    
    if (grepl("^b", side))
        return(unique(c(set1, set2)))
    
    if (grepl("^l", side))
        return(set1)
    
    if (grepl("^r", side))
        return(set2)
}



#' Count/proportion of `NA`s per dataframe row
#'
#' @param df (Dataframe) A dataframe.
#' @param ... (Tidy-select) `dplyr`-style column selection. 
#'     See [dplyr::dplyr_tidy_select()]. If empty, defaults to `dplyr::everything()`.
#'
#' @return The dataframe `df` with two new columns: `na_in_row_count` and `na_in_row_prop`.
#' @export
#'
#' @examples
#' df <- data.frame(a = c(1, 2, NA, 4, NA), b = 1:5, c = c(NA, 2, 3, NA, NA))
#' 
#' df
#' 
#' #>     a b  c
#' #> 1   1 1 NA
#' #> 2   2 2  2
#' #> 3  NA 3  3
#' #> 4   4 4 NA
#' #> 5  NA 5 NA
#' 
#' # By default, looks for NAs in all columns
#' na_in_row(df)
#' 
#' #>    a b  c na_in_row_count na_in_row_prop
#' #> 1  1 1 NA               1      0.3333333
#' #> 2  2 2  2               0      0.0000000
#' #> 3 NA 3  3               1      0.3333333
#' #> 4  4 4 NA               1      0.3333333
#' #> 5 NA 5 NA               2      0.6666667
#' 
#' # Or use tidyselectors to choose columns. Here, looking for 
#' # NAs in all columns except `b`
#' na_in_row(df, -b)
#' 
#' #>    a b  c na_in_row_count na_in_row_prop
#' #> 1  1 1 NA               1            0.5
#' #> 2  2 2  2               0            0.0
#' #> 3 NA 3  3               1            0.5
#' #> 4  4 4 NA               1            0.5
#' #> 5 NA 5 NA               2            1.0
#' 
#' @section Source:
#' - <https://stackoverflow.com/a/35444245/5578429>
#' 
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' - maloneypatr (<https://stackoverflow.com/users/2124146/maloneypatr>)
#' 
#' @md
na_in_row <- function(df, ...) {
    if (...length() == 0) {
        wip <- dplyr::select(df, everything())
    } else {
        wip <- dplyr::select(df, ...)
    }
    
    wip <- dplyr::mutate(wip,
                         na_in_row_count = apply(wip, 1, function(x) sum(is.na(x))),
                         na_in_row_prop  = na_in_row_count / apply(wip, 1, length))
    
    return(dplyr::bind_cols(df, 
                            dplyr::select(wip, na_in_row_count, na_in_row_prop)))
}

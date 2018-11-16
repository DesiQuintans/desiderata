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
#'
#' @examples
#' \dontrun{
#' cols <- construct_cols(mtcars, from = 7)
#' cols
#'
#' #> [1]  7  8  9 10 11
#'
#' mtcars[cols]
#'
#' #>                      qsec vs am gear carb
#' #> Mazda RX4           16.46  0  1    4    4
#' #> Mazda RX4 Wag       17.02  0  1    4    4
#' #> Datsun 710          18.61  1  1    4    1
#' #> Hornet 4 Drive      19.44  1  0    3    1
#' }
#'
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



#' Find the nearest rectangle that will fit the number of entries in a vector
#'
#' This function is used by `show_colours()` to find the dimensions of a grid (as close
#' to square as possible) that will fit all of the entries in a vector of colours.
#'
#' @param vec (Vector) A vector.
#'
#' @return A named numeric vector with 2 entries: `x` and `y` describing the number of
#'    columns and rows respectively.
#'
#' @examples
#' \dontrun{
#' find_dims(1:5)
#'
#' #> x y
#' #> 2 3
#' }
#'
#' @md
find_dims <- function(vec)  {
    cells <- length(vec)

    # I only have a method for building a square right now.
    x <- floor(sqrt(cells))
    y <- ceiling(cells / x)

    return(c("x" = x, "y" = y))
}



#' Build a palette of colours from a list of hex codes
#'
#' @param col_list (Character) A vector of colours in RGB Hex format without transparency.
#' @param n (Numeric or `NULL`) The number of colours to return. If `NULL`, return all of
#'    the colours for `col_list`.
#' @param random (Logical) If `TRUE`, colours will be randomly sampled without replacement.
#'    If `FALSE`, they will be drawn from `col_list` in order.
#' @param alpha (Numeric) A single decimal value that modifies the opacity of the colours.
#'    `alpha = 0.65` makes all of the colours 65 percent opaque (35 percent transparent).
#'    If `alpha = NULL`, no alpha channel is added and the colours are 100 percent opaque.
#' @param spaced (Logical) If `TRUE`, `n` colours will be chosen equally spaced within the
#'    `col_list`, e.g. 3 colours from a list of 16 will return 1, 8, and 16th colours.
#'    If `FALSE` (by default), colours will not be spaced out.
#'
#' @return A character vector of hex colours. If `alpha = NULL`, the colours will be in
#'    RGB Hex format (e.g. #FFFF00). If `alpha` is not `NULL`, the colours will be in
#'    RGBA Hex format (e.g. #FFFF00CB).
#'
#' @examples
#' \dontrun{
#' build_palette(c("#000000", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46"), alpha = 0.5)
#' #> [1] "#00000080" "#FFFF0080" "#1CE6FF80" "#FF34FF80" "#FF4A4680"
#' }
#'
#' @md
build_palette <- function(col_list, n = NULL, random = FALSE, spaced = FALSE, alpha = NULL) {
    # Default to returning all of the colours in the list if no specific number is chosen,
    # or if too many colours are requested.
    list_length <- length(col_list)

    if (is.null(n) == TRUE) {
        n <- list_length
    } else if (n <= 0 | is.numeric(n) == FALSE) {
        stop("You need to request at least 1 colour, or leave the 'n' argument as NULL\n",
             "  to request all colours by default.")
    } else if (list_length < n) {
        warning(n, " colours were requested, but there are only ", list_length, " colours ",
                "in the list. \n",
                "  All ", list_length, " colours will be returned.")
        n <- list_length
    }

    # 1. Build transparency info
    if (is.null(alpha) == TRUE) {
        alpha_data <- ""
    } else {
        # The decimal is converted to hex and then appended to the end of the colour code.
        alpha_data <- format(as.hexmode(round(255 * alpha[1])), upper.case = TRUE)
    }

    # 2. Randomise colours if needed.
    if (random == TRUE) {
        cols <- sample(col_list)
    } else {
        cols <- col_list
    }

    # 2. Then, take every nth element from the list.
    if (spaced == TRUE) {
        nth <- floor(seq(1, list_length, length.out = n))  # n elements, spaced evenly.

        # Note that n is never greater than list_length because of the length check at the
        # start of the function, so this function will return every 1th element if too
        # many n are requested.
    } else {
        nth <- seq(1, n)  # Return every 1th element up to n elements.
    }

    # 3. Select the colours
    cols <- cols[nth]

    # paste() destroys any names that are attached to the list. Need to save them first
    # and reapply them later.
    col_names <- names(cols)
    cols <- paste0(cols, alpha_data)
    names(cols) <- col_names

    return(cols)
}

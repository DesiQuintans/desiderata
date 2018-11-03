# Functions for working with strings


#' Collapse vectors into a regex pattern
#'
#' @param ... (Vector) Vectors that will be coerced into character. Duplicated elements
#'     from the vectors are removed.
#' @param sep (Char) Separator to use between elements of ...
#' @param wrap (Char) 2-element vector of text to wrap the pattern with. `wrap[1]` is
#'     placed on the left side of the pattern, `wrap[2]` is placed on the right side.
#'
#' @return A character string where every element of `...` is collapsed with `sep`, and
#'     wrapped with `wrap`.
#' @export
#'
#' @examples
#' vec_to_regex(month.abb)
#' #> [1] "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"
#'
#' vec_to_regex(letters[1:6], sep = "", wrap = c("([", "]+)"))
#' #> [1] "([abcdef]+)"
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
vec_to_regex <- function(..., sep = "|", wrap = c("(", ")")) {
    # I run unique() here so that I don't have to do it on every single vector that I put
    # into ... when I call the function.
    needles <- unique(as.character(c(...)))

    pattern = paste0(wrap[1], paste(needles, collapse = sep), wrap[2])

    return(pattern)
}



#' Collapse a vector into a string
#'
#' Take the elements of multiple vectors and concatenate them into one long string.
#'
#' @param ... (...) Vectors that will be concatenated and coerced to Character.
#' @param wrap (Character) Placed at the left and right sides of each vector element.
#' @param collapse (Character) Placed between each element of the original vector(s).
#' @param unique (Logical) If `TRUE`, duplicate entries in `...` will be removed.
#'
#' @return A string.
#'
#' @examples
#' \dontrun{
#' collapse_vec(month.abb, wrap = "-", collapse = ", ")
#' #> [1] "-Jan-, -Feb-, -Mar-, -Apr-, -May-, -Jun-, -Jul-, -Aug-, -Sep-, -Oct-, -Nov-, -Dec-"
#' }
#'
#' @md
collapse_vec <- function(..., wrap = "'", collapse = ", ", unique = TRUE) {
    vec <- as.character(c(...))

    if (unique == TRUE) {
        vec <- unique(vec)
    }

    # vec is wrapped in empty strings so that 'sep' arg will wrap each entry.
    paste(character(0), vec, character(0), collapse = collapse, sep = wrap)
}

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
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
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



#' 'Unwrap' strings by ignoring hard-wrapping from the source code
#'
#' It is often necessary to break a very long string (e.g. a table caption) across
#' several lines in your source code to keep it readable, but these linebreaks and
#' spaces end up being printed in the final output. This function removes hard-wrap
#' whitespace characters while preserving the ones you explicitly add with `\\n`.
#'
#' @details `uw()` will replace any linebreak that is followed by any number of
#'    spaces with a single space. This means that **if you want to insert a linebreak
#'    `\\n` manually, then it should not have any spaces after it**. A `\\n` at the
#'    very end of the line will be kept, and this is the most sensible way to format
#'    the text anyway.
#'
#'    Also note that since `uw()` uses the presence of indenting spaces to decide
#'    whether a piece of text is hard-wrapped, text that merely goes to the 0th
#'    column is not unwrapped. Compare:
#'
#'        text <- "This will be
#'                unwrapped by uw()."
#'
#'        text <- "This will NOT
#'        be unwrapped by uw()."
#'
#' @param ... (Character) Vectors that you want to collapse into a single string.
#'    They will be coerced to `Character`.
#' @param collapse (Character) All of the elements in `...` will be joined together,
#'    with `collapse` as the separator between them.
#'
#' @return A string with all elements in `...` joined together and separated
#'    with `collapse`. Linebreaks that **are not** immediately followed by one or
#'    more spaces will be kept.
#' @export
#'
#' @examples
#' text <- "Here's an example of some text
#'         that you might want to break
#'         across many lines.\n
#'         But this line should be separate."
#'
#' print(text)
#'
#' #> [1] "Here's an example of some text\n        that you might want to break..."
#'
#' cat(text)
#'
#' #> Here's an example of some text
#' #>         that you might want to break
#' #>         across multiple lines.
#' #>
#' #>         But this line should be separate.
#'
#' uw(text)
#'
#' #> [1] "Here's an example of some text that you might want to break across many..."
#'
#' cat(.Last.value)
#'
#' #> Here's an example of some text that you might want to break across many lines.
#' #> But this line should be separate.
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
uw <- function(..., collapse = " ") {
    no_hardwrap <-
        gsub("\\h?\\R{1}\\h+", " ",  # Trim newline/whitespace from hard-wrapping.
             paste(c(...), collapse = collapse),
             perl = TRUE)  # Perl for \h and \R.

    # Any \n left behind was put there by the user.
    no_space_after_n <-
        gsub("\\h?(\\R{1})\\h+", "\\1",
             no_hardwrap,
             perl = TRUE)

    return(no_space_after_n)
}



#' Return the stem that is common to a set of strings
#'
#' @param ... (Vectors) Vectors that will be coerced into Character and
#'    appended together.
#' @param side (Character) Search from the `left` or `right` side of the strings.
#'    Also accepts `l` or `r` as shorthand.
#' @param na.rm (Logical) Should `NA` be removed from the input vectors?
#'
#' @return A string. If there is no common stem among all the words, an empty string
#'    of length 1 ("") will be returned.
#' @export
#'
#' @examples
#' vec <- c("exciting", "exceeding", "excepting")
#'
#' common_stem(vec)
#' #> [1] "exc"
#'
#' common_stem(vec, side = "r")
#' #> [1] "ing"
#'
#' # The function does not return substrings:
#' common_stem("tableaux", "wobbles")
#' #> ""
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
common_stem <- function(..., side = "left", na.rm = FALSE) {
    vec <- as.character(c(...))
    if (na.rm == TRUE) { vec <- vec[!is.na(vec)] }

    min_length  <- min(nchar(vec))
    num_entries <- length(vec)

    if (is.na(min_length) | min_length <= 0) {
        stop("The shortest vector element must be at least 1 character long.")
    }

    if (side == "l" || side == "left") {
        stem_side <- "left"
    } else if (side == "r" || side == "right") {
        stem_side <- "right"
    } else {
        stop("The 'side' argument must be 'left' or 'right'.")
    }

    # If side = right, the string needs to be reversed now before truncating.
    if (stem_side == "right") vec <- str_rev(vec)

    # The strings get truncated to the length of the shortest element because the
    # common substring between them can't be any longer than the shortest string.
    # This also lets me break the vector into a rectangular matrix.
    trunc <- strtrim(vec, min_length)
    trunc <- strsplit(trunc, "")
    trunc <- unlist(trunc)

    mat   <- matrix(trunc, ncol = min_length, nrow = num_entries, byrow = TRUE)

    output <- ""

    # I start from the left because the stem will probably end sooner rather
    # than later.
    for (i in 1:min_length) {
        if (length(unique(mat[,i])) == 1) {
            output <- paste0(output, mat[1,i])
        } else {
            break
        }
    }

    if (stem_side == "right") { output <- str_rev(output) }

    return(output)
}



#' Reverse every string in a vector of strings
#'
#' @param ... (Vectors) Vectors that will be coerced to Character and joined together.
#' @param USE.NAMES (Logical) Should the output be a named vector, or unnamed?
#'
#' @return A Character vector where every element from the original vectors have been
#'    reversed. NAs remain as NA.
#' @export
#'
#' @examples
#' vec <- c("Beret", "Clipper", "Cornet", NA)
#' str_rev(vec)
#'
#' #> [1] "tereB"   "reppilC" "tenroC"
#'
#' @section Authors:
#' - Kevin Ushey (<https://stackoverflow.com/users/1342082/kevin-ushey>)
#'
#' @section Source:
#' <https://stackoverflow.com/a/14029000/5578429>
#'
#' @md
str_rev <- function(..., USE.NAMES = FALSE) {
    sapply(as.character(c(...)),
           # Expand each element into its own vector of code points, then reverse
           # that vector, then convert it from code points back to text.
           function(x) intToUtf8(rev(utf8ToInt(x))),
           USE.NAMES = USE.NAMES)
}

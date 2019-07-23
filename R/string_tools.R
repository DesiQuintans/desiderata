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
#' It is often necessary to break a very long string (e.g. a figure caption) across
#' several lines in your source code to keep it readable. In base R, you do this by
#' breaking the caption into several substrings and `paste()`ing them together, which
#' is kind of clumsy. This function lets you write a single string that is visually
#' hard-wrapped with newlines and spaces, but removes those whitespace characters
#' from the final output.
#'
#' @details `uw()` will take any linebreak that is followed by any number of spaces
#'   and replace it with a single space (or whatever string you specify in the `join`
#'   argument). This means that **if you want to insert a linebreak `\\n` manually,
#'   then it should not have any spaces after it**. A `\\n` that is at the very end
#'   of a line will be kept.
#'
#'   Also note that since `uw()` uses the presence of indenting spaces to decide
#'   whether a piece of text is hard-wrapped, text that merely goes to the 0th column
#'   is not unwrapped. Compare:
#'
#'     text <- "This will be
#'             unwrapped by uw()."
#'
#'     text <- "This will NOT
#'     be unwrapped by uw()."
#'
#' @param ... (Character) Vectors that you want to collapse into a single string.
#'    They will be coerced to `Character`.
#' @param collapse (Character) All of the elements in `...` will be joined together,
#'    with `collapse` as the separator between them.
#' @param join (Character) Separate lines will be joined with this string.
#'
#' @return A string with all elements in `...` joined together and separated
#'    with `collapse`. Linebreaks that **are not** immediately followed by one or
#'    more spaces will be kept.
#' @export
#'
#' @examples
#' text <- "Here's some
#'          multi-line text.\n
#'          This is on a new line."
#'
#' print(text)
#'
#' #> [1] "Here's some\n         multi-line text.\n\n         This is on a new line."
#'
#' cat(text)
#'
#' #> Here's some
#' #>          multi-line text.
#' #>
#' #>          This is on a new line.
#'
#' uw(text)
#'
#' #> [1] "Here's some multi-line text.\nThis is on a new line."
#'
#' cat(.Last.value)
#'
#' #> Here's some multi-line text.
#' #> This is on a new line.
#'
#' uw(text, join = "##")
#' #> [1] "Here's some##multi-line text.\n##This is on a new line."
#'
#' cat(.Last.value)
#' #> Here's some##multi-line text.
#' #> ##This is on a new line.
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
uw <- function(..., collapse = " ", join = " ") {
    no_hardwrap <-
        gsub("\\h?\\R{1}\\h+", join,  # Trim newline/whitespace from hard-wrapping.
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



#' Reverse every element of a vector
#'
#' This reverses the individual elements (makes words go backwards, for example),
#' but does not change the order of those elements in the vector.
#'
#' @param ... (Vectors) Vectors that will be coerced to Character and joined together.
#' @param USE.NAMES (Logical) Should the output be a named vector, or unnamed?
#'
#' @return A Character vector where every element from the original vectors has been
#'    reversed, but the order of the elements within that vector is still the same.
#'    NAs remain as NA.
#' @export
#'
#' @examples
#' vec <- c("Beret", "Clipper", "Cornet", NA)
#' str_rev(vec)
#'
#' #> [1] "tereB"   "reppilC" "tenroC"  NA
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



#' Get the nth word from a vector of strings
#'
#' @param vec (Character) A vector of strings. It will be coerced to Character.
#' @param n (Integer) Which word will be retrieved? Positive integers count from 
#'    the start of the string, negative integers count from the end.
#' @param split (Character) A regular expression that matches word boundaries.
#'    By default, it matches spaces and sentence-ending punctuation. See 
#'    \link[base]{strsplit}.
#'
#' @return A character vector that is the same length as `vec`. For `vec` 
#'    elements where no word was found (e.g. `NA` or an empty string `""`), `NA`
#'    will be returned.
#' @export
#'
#' @examples
#' vec <- c("Lorem ipsum dolor",
#'          "sit amet, consectetur",
#'          "adipiscing elit, sed",
#'          "do eiusmod tempor",
#'          "incididunt ut labore",
#'          "et dolore magna",
#'          "aliqua.")
#' 
#' nth_word(vec, n = 1)
#' 
#' ## [1] "Lorem"  "sit"  "adipiscing"  "do"  "incididunt"  "et"  "aliqua"
#' 
#' nth_word(vec, n = -1)
#' 
#' ## [1] "dolor"  "consectetur"  "sed"  "tempor"  "labore"  "magna"  "aliqua" 
#' 
#' nth_word("Order_Family_Genus", n = 2, split = "_")  
#' 
#' ## [1] "Family"
#' 
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' 
#' @md
nth_word <- function(vec, n = 1, split = "(\\.|!|\\?){0,}( |$)") {
    vec <- as.character(vec)
    
    get_word <- function(str, n) {
        if (n == 0) {
            stop("'n' should be a positive integer (to get the first words) or 
                 a negative integer (to get the last words).")
        }
        
        if (n < 0) {
            n <- (length(str) + 1) + n
        }
        
        str[n]
    }
    
    unlist(lapply(strsplit(vec, split), get_word, n))
}



#' Reverse the order of words in a string
#'
#' @param vec (Character) The vector to reverse.
#' @param split (Character) A regular expression that determines where `vec` will be 
#'    split into separate words. By default, splits at spaces and underscores.
#' @param join (Character) A string that will be inserted between the words.
#'
#' @return A character vector that is the same length as `vec`.
#' @export
#'
#' @examples
#' vec <- c("Lorem ipsum dolor",
#'          "sit amet, consectetur",
#'          "adipiscing elit, sed",
#'          "do eiusmod tempor",
#'          "incididunt ut labore",
#'          "et dolore magna",
#'          "aliqua.")
#' 
#' rev_sentence(vec)
#' 
#' ## [1] "dolor ipsum Lorem"  "consectetur amet, sit"  "sed elit, adipiscing"  
#' ## [4] "tempor eiusmod do"  "labore ut incididunt"  "magna dolore et"  "aliqua."   
#' 
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' 
#' @md
rev_sentence <- function(vec, split = " |_", join = " ") {
    res <- strsplit(vec, split)
    res <- sapply(res, rev)
    res <- sapply(res, paste, collapse = join)
    
    return(res)
}

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
#' @param between (Character) Placed between each element of the original vector(s).
#' @param unique (Logical) If `TRUE`, duplicate entries in `...` will be removed.
#'
#' @return A string.
#'
#' @examples
#' \dontrun{
#' vec <- c("Gum", "Rock", "Pen", "Bottle", "Pen")
#' 
#' str_coll(vec)
#' #> [1] "Gum,Rock,Pen,Bottle,Pen"
#' 
#' str_coll(vec, wrap = "-", between = ", ")
#' #> [1] "-Gum-, -Rock-, -Pen-, -Bottle-, -Pen-"
#' 
#' str_coll(vec, wrap = "-", between = ", ", unique = TRUE)
#' #> [1] "-Gum-, -Rock-, -Pen-, -Bottle-"
#' }
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
str_coll <- function(..., wrap = "", between = ",", unique = FALSE) {
    vec <- as.character(c(...))

    if (unique == TRUE) {
        vec <- unique(vec)
    }

    # vec is wrapped in empty strings so that 'sep' arg will wrap each entry.
    paste(character(0), vec, character(0), collapse = between, sep = wrap)
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
#'   If you want to unwrap without adding spaces at all, use `uw0()`, which is a
#'   shortcut for `uw(..., collapse = "", join = "")`.
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


#' @rdname uw
#' @examples
#' long_url <- "http://www.long-api-query.com/ask?
#'              q=question&
#'              n=200&
#'              pg=3"
#' 
#' cat(long_url)
#' #> http://www.long-api-query.com/ask?
#' #>     q=question&
#' #>     n=200&
#' #>     pg=3
#' 
#' uw0(long_url)
#' #> [1] "http://www.long-api-query.com/ask?q=question&n=200&pg=3"
#' @export
uw0 <- function(...) {
    uw(..., collapse = "", join = "")
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



#' Interleave one string with another
#' 
#' Interleave a string with another string. By default, interleaves newlines (`\\n`) 
#' between each letter to help format a compact letter display.
#'
#' @param str (Character) The string to be split into sections.
#' @param split (Character) Regular expression to split `str` by. If its length is
#'     zero (the default), it will split at every character.
#' @param insert (Character) The string to be inserted between sections.
#'
#' @return A Character vector.
#' @export
#'
#' @examples
#' interleave(c("hello", "hey"), "-")
#' 
#' ## [1] "h-e-l-l-o" "h-e-y" 
#' 
#' interleave(c("hello", "hey"), insert = "|_|", split = "el")
#' ## [1] "h|_|lo" "hey"  
#' 
#' interleave(c(1234, 9876), "-")
#' 
#' ## [1] "1-2-3-4" "9-8-7-6"
#' 
#' @md
interleave <- function(str, insert = "\n", split = character(0)) {
    sapply(strsplit(as.character(str), as.character(split)), 
           paste, collapse = as.character(insert))
    
}



#' Match strings that have a needle near the start, end, or middle
#'
#' @param str (Character) The vector to be searched.
#' @param query (Character) The regular expression to look for. If `length(query) > 1`, 
#'     will be collapsed into a regular expression as `"(item1|item2|item3...)"`.
#' @param buffer (Numeric) The length of the substring to search. If given as a whole number, 
#'     it will be that many characters long. If given as a decimal number, it will be used 
#'     as a proportion of the length of each element in `str`, e.g. `buffer = 0.20` is 20% 
#'     of each element's length.
#' @param from (Character) If `"start"` or `"s"` (default), the search will be done from 
#'     the start of each string. If `"end"` or `"e"`, it will be from the end. If `"middle"` 
#'     or `"m"`, the middle of the string will be searched.
#' @param values (Logical) If `FALSE` (default), returns a Logical vector of whether a match 
#'     was found in each element of `str`. If `TRUE`, returns a Character vector containing
#'     only the elements of `str` that matched.
#'
#' @return A Logical vector if `values == FALSE` (default), or a Character vector 
#'     if `values == TRUE`.
#' @export
#'
#' @examples
#' sentences <- c(
#'     "The word 'needle' appears at the start of this sentence.",
#'     "But in this sentence, 'needle' doesn't.",
#'     "If 'needle' appears several times in a sentence, then we have a lot of needles!",
#'     "And in here, the word we want to find (needle) is near the middle of the sentence."
#'     )
#' 
#' # Within 20 characters of the Start of the string
#' match_in_substr(sentences, "needle", 20, "s")
#' 
#' #> [1] TRUE FALSE TRUE FALSE
#' 
#' # In the last 25% of the string
#' match_in_substr(sentences, "needle", 0.25, "e")
#' 
#' #> [1] FALSE FALSE TRUE FALSE
#' 
#' # In the middle 1/3rd of the string
#' match_in_substr(sentences, "needle", 1/3, "m")
#' 
#' #> [1] FALSE FALSE FALSE TRUE
#' 
#' match_in_substr(sentences, "needle", 1/3, "m", values = TRUE)
#' #> [1] "And in here, the word we want to find (needle) is near the middle of the sentence."
#' 
#' @md
#' @importFrom magrittr %>%
match_in_substr <- function(str, query, buffer = 0.25, from = "s", values = FALSE) {
    # From ?integer, because is.integer(5) returns FALSE.
    is.wholenumber <-
        function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    
    # Build the query
    needle <- ifelse(length(query) == 1,
                     paste0("(", query, ")"),
                     paste0("(", paste(query, collapse = "|"), ")"))
    
    df <- 
        str %>% 
        # Split str into sentences
        # stringr::str_split(stringr::boundary("sentence")) %>%
        # unlist() %>%
        dplyr::as_tibble() %>% 
        dplyr::rename(orig = 1) %>% 
        dplyr::mutate(orig = stringr::str_trim(orig)) %>%
        # Calculate per-sentence buffer lengths if buffer is a double. An intention like
        # "Within the first 10% of the sentence" is calculated for every sentence length.
        dplyr::mutate(sent_len = stringr::str_length(orig),
                      buffer_len = dplyr::case_when(is.wholenumber(buffer) ~ buffer,
                                                    TRUE ~ ceiling(buffer * sent_len))) %>%
        # Truncate each sentence to only keep the search area. I do this so that searches
        # in the middle of a string work properly for sentences of any length.
        dplyr::mutate(sent_trunc = dplyr::case_when(
            grepl("^s|S", from) ~ stringr::str_sub(orig, 1, buffer_len),
            grepl("^e|E", from) ~ stringr::str_sub(orig, -buffer_len, -1),
            grepl("^m|M", from) ~ stringr::str_sub(orig, buffer_len, -buffer_len)
        )) %>%
        # Do the search
        dplyr::mutate(found = stringr::str_detect(sent_trunc, needle))
    
    # Return the matches
    if (values == FALSE) {
        return(df$found)
    } else {
        df %>% 
            dplyr::filter(found == TRUE) %>% 
            purrr::pluck("orig") %>% 
            return()
    }
}

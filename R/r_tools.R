# Functions for writing R code

#' Suppress all console printing (cat, print, warning, message)
#'
#' Sometimes developers leave debugging messages in their packages or, infuriatingly,
#' choose to output messages by using print() or cat() instead of message() or warning()
#' like they're supposed to. This function suppresses them to remove that clutter.
#' 
#' If you want to catch errors easily, I recommend `possibly()` or `safely()` or
#' `quietly()` in the `purrr` package. 
#'
#' @param x (Expression) An expression, usually a call to a function.
#'
#' @return The returned value of the expression.
#' @export
#'
#' @examples
#' loud_mean <- function(x) {
#'     print("This is from print().")
#'     cat("This is from cat().\n")
#'     message("This is from message().")
#'     warning("This is from warning().")
#'     mean(x)
#' }
#'
#' loud_mean(1:100)
#'
#' #> [1] "This is from print()."
#' #> This is from cat().
#' #> This is from message().
#' #> [1] 50.5
#' #> Warning message:
#' #>     In loud_mean(1:100) : This is from warning().
#'
#' shush(loud_mean(1:100))
#'
#' #> [1] 50.5
#'
#' # magrittr pipelines will also work.
#' # shush(loud_mean(1:100) %>% sqrt())
#' #> [1] 7.106335
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' - hplieninger (<https://stackoverflow.com/users/2563804/hplieninger>)
#'
#' @section Source:
#' <https://stackoverflow.com/a/48503375/5578429>
#'
#' @md
shush <- function(x) {
    call <- quote(x)

    invisible(
        utils::capture.output(
            out <- suppressWarnings(suppressMessages(eval(call))))
        )

    # Has to return invisible() because of an edge case where if the shushed function
    # returned an invisible value, that value would be printed out by capture.output().
    #
    # testfun <- function(x) return(invisible("Should be invisible"))
    # shush(testfun())
    # [1] "Should be invisible"
    return(invisible(out))
}



#' Print to console, wrapping the text to a specific line width
#'
#' Wrapping console output is essential in Rmarkdown documents because long character
#' vectors do not wrap when printed inside code blocks.
#'
#' @param text (Character) A vector of text.
#' @param width (Integer) The character length of each line.
#' @param element_sep (Character) A string to insert between each element of `text`.
#'
#' @return Print to console.
#' @export
#'
#' @examples
#' vec <- c("This is a very long chunk of text.",
#'          "This is also another quite long chunk of text.")
#'
#' cat_wrap(vec, width = 25)
#'
#' #> This is a very long
#' #> chunk of text.
#' #>
#' #> This is also another
#' #> quite long chunk of
#' #> text.
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
cat_wrap <- function(text, width = 80, element_sep = "\n") {
    # Inter-element separator can't be "\n" because strwrap() discards whitespace. Here
    # I use the Pilcrow character as a unicode escape sequence. I know that user text
    # may contain a Pilcrow, but using a very long marker (e.g. "#ELEMENT-SEP-HERE#")
    # makes the string longer and can mess with how the lines get wrapped. To avoid
    # replacing Pilcrows that might be in user text, I only search for the one that's at
    # the end of the line.
    wrap <- strwrap(paste0(text, "\u00B6"), width = width, prefix = "\n", initial = "")

    separated <- gsub("\u00B6$", element_sep, wrap)  # Now I can insert whitespace.

    return(cat(separated))
}



#' Randomly return TRUE or FALSE
#'
#' @param n (Integer) The number of times to 'flip the coin'
#'
#' @return Returns `TRUE` or `FALSE` randomly.
#' @export
#'
#' @examples
#' coinflip()
#' #> [1] FALSE
#'
#' coinflip(5)
#' #> [1] TRUE TRUE FALSE TRUE FALSE
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
coinflip <- function(n = 1) {
    sample(c(TRUE, FALSE), size = n, replace = TRUE)
}



#' Convert dots (...) to a character vector or a string
#'
#' @param ... (Dots) Elements in dots.
#' @param collapse (Character or `NULL`) The elements of `...` will be `paste`d
#'   together with this string between them.
#'
#' @return A character vector. If `collapse = NULL`, returns each separate item in
#'   `...` as a separate element of a character vector. If `collapse = "-"`, for
#'   example, returns a single string where all items in `...` are joined together
#'   with `-`.
#' @export
#'
#' @examples
#' dots_char(return, this, as, a, vector)
#' 
#' #> [1] "return" "this" "as" "a" "vector" 
#' 
#' dots_char(return, this, as, a, single, string, collapse = "_")
#' 
#' #> [1] "return_this_as_a_single_string"
#'
#' @md
dots_char <- function(..., collapse = NULL) {
    paste(as.character(eval(substitute(alist(...)))), 
          collapse = collapse)
}



#' Remove NAs from vectors, lists, matrices, and dataframes
#'
#' This is a wrapper around `stats::na_omit()` which hides its annoying printing 
#' behaviour when applied to a vector.
#'
#' @param obj (Vector/List/Matrix/Dataframe) The thing to remove `NA`s from.
#'
#' @return A copy of `obj` without NAs.
#' @export
#'
#' @examples
#' na_rm(c("a", NA, "b", NA, "c", "d"))
#' 
#' #> [1] "a" "b" "c" "d"
#' 
#' 
#' na_rm(data.frame(col1 = 1:5, col2 = c("a", NA, "c", "d", NA)))
#' 
#' #>   col1 col2
#' #> 1    1    a
#' #> 3    3    c
#' #> 4    4    d
#'
#' @md
na_rm <- function(obj) {
    na_removed <- stats::na.omit(obj)
    
    if (is.vector(obj)) {
        # The na.omit() method for a vector prints a lot of guff:
        #
        #     # > y <- na.omit(c("a", NA, "b", NA, NA, "c"))
        #     # > y
        #     # [1] "a" "b" "c"
        #     # attr(,"na.action")
        #     # [1] 2 4 5
        #     # attr(,"class")
        #     # [1] "omit"
        #
        # By setting the resulting object's attributes to NULL, it converts the
        # output into a regular ol' vector. This does not affect the output for a 
        # list that is run through this function.
        attributes(na_removed) <- NULL  
    }
    
    return(na_removed)
}



#' Seed the random number generator with a character string (or any object)
#'
#' Set the random number generator's seed using a string if you want to be extra cute and
#' use your cats' names like I do. This function can actually generate a seed from any R
#' object, so you could even feed it a whole dataframe if you felt like it. (Requires the
#' ['digest'](https://cran.r-project.org/web/packages/digest/index.html) package.)
#'
#' @param seed (Any) Any object.
#'
#' @return `NULL`.
#' @export
#'
#' @examples
#' # set_seed_any("Snake... Do you think love can bloom, even on a battlefield?")
#'
#' # set_seed_any(iris)
#'
#' @section Authors:
#' - Ben Bolker (<https://stackoverflow.com/users/190277/ben-bolker>)
#'
#' @section Source:
#' <https://stackoverflow.com/a/10913336/5578429>
#'
#' @md
set_seed_any <- function(seed) {
    digest_installed <- "digest" %in% rownames(utils::installed.packages())
    if (digest_installed == FALSE) {
        stop("The 'digest' package needs to be installed.")
    }
    
    hexval <- paste0("0x", digest::digest(seed, "crc32"))
    intval <- utils::type.convert(hexval) %% .Machine$integer.max
    set.seed(intval)
}



#' Quick assertion function for sanity checking your code
#'
#' This function can be used to do quick checks on your code, especially in 
#' RMarkdown analysis documents. For example, making sure that the number of 
#' rows in a dataframe hasn't changed after a left-join operation. For more 
#' than a handful of tests, look for a fuller testing package like `assertr`.
#'
#' @param desc (Character) Human-readable description of the expression 
#'     being evaluated.
#' @param expr (Expression) The expression being tested. It should evaluate to
#'     `TRUE` when the test passes, and `FALSE` when the test fails.
#' 
#'
#' @return An Error if the test fails, or a Message if the test passes.
#' @export
#'
#' @examples
#' \dontrun{
#' letters
#' length(letters)
#' 
#' ensure("There should be 26 letters", length(letters) == 26)
#' }
#' 
#' @md
ensure <- function(desc, expr) {
    if (eval(expr) == FALSE) {
        stop('"', desc, '"', " fails QA check (FALSE).")
    } else {
        message('"', desc, '"', " passes QA check (TRUE).")
    }
}

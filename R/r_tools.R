# Functions for writing R code

#' Suppress all console printing (cat, print, warning, message)
#'
#' Sometimes developers leave debugging messages in their packages, and you don't want
#' them cluttering your Rmarkdown document. This suppresses them.
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

    return(out)
}


#' Inverse match
#'
#' Flags the elements of x that are not in y.
#'
#' @name notin
#' @usage x \%notin\% y
#'
#' @param x (Vector) The values to be matched. Long vectors (2^31 elements) are supported.
#' @param y (Vector) The values to be matched against. Long vectors are not supported.
#'
#' @return A logical vector of the same length as `x`, with `TRUE` if the element was
#'    found in `y`, and `FALSE` if it was not.
#' @export
#'
#' @examples
#' c(1, 4, 21, 7, -3) %in% 0:10
#' #> [1]  TRUE  TRUE FALSE  TRUE FALSE
#'
#' c(1, 4, 21, 7, -3) %notin% 0:10
#' #> [1] FALSE FALSE  TRUE FALSE  TRUE
#'
#' @section Authors:
#' - R Core Team (<https://www.r-project.org/contributors.html>)
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
"%notin%" <- function(x, y) {
    !(match(x, y, nomatch = 0) > 0)
}



#' Percentage of matching elements between two vectors
#'
#' Find the percentage of elements in x that are present in y.
#'
#' @name pctin
#' @usage x \%pctin\% y
#'
#' @param x (Vector) The values to be matched.
#' @param y (Vector) The values to be matched against.
#'
#' @return The percentage of elements in x that are present in y.
#' @export
#'
#' @examples
#' c(1, 4, 21, 7, -3) %in% 0:10
#' #> [1]  TRUE  TRUE FALSE  TRUE FALSE
#'
#' c(1, 4, 21, 7, -3) %pctin% 0:10
#' #> [1] 0.6
#'
#' @section Authors:
#' - GSee (<https://stackoverflow.com/users/967840/gsee>)
#'
#' @section Source:
#' <https://stackoverflow.com/a/13830068/5578429>
#'
#' @md
"%pctin%" <- function(x, y) {
    length(x[x %in% y])/length(x)
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



# Functions for writing R code

#' Suppress all console printing (cat, print, warning, message)
#'
#' Sometimes developers leave debugging messages in their packages or, infuriatingly,
#' choose to output messages by using print() or cat() instead of message() or warning()
#' like they're supposed to. This function suppresses them to remove that clutter.
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


#' Copy a dataframe, vector, or the result of an expression to the clipboard
#'
#' Tested on Windows.
#'
#' @param x (An expression, vector, dataframe, or `NULL`) The thing to copy to the
#'   clipboard. If `NULL`, the `.Last.value` will be copied. If x is a dataframe, it will
#'   be copied with column names but not row names. If x is something else (a vector, for
#'   example) it will be coerced into a one-column dataframe and copied **without** column
#'   names or row names. Char and factor in x will not be surrounded by quotes in the
#'   clipboard.
#'
#' @return Invisibly returns the contents of the clipboard. If `.Last.value` is displayed
#'   in RStudio's 'Environment' tab, you'll see its value change.
#' @export
#'
#' @examples
#' clippy(mtcars)
#'
#' # Clipboard contents:
#' # mpg  cyl disp  hp   drat  wt     qsec   vs  am  gear  carb
#' # 21   6   160   110  3.9   2.62   16.46  0   1   4     4
#' # 21   6   160   110  3.9   2.875  17.02  0   1   4     4
#' # ...
#'
#' clippy(iris$Petal.Length)
#'
#' # Clipboard contents:
#' # 1.4
#' # 1.4
#' # 1.3
#' # ...
#'
#' clippy(colnames(iris))
#'
#' # Clipboard contents:
#' # Sepal.Length
#' # Sepal.Width
#' # Petal.Length
#' # Petal.Width
#' # Species
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' - Darwin PC (<https://stackoverflow.com/users/2543306>)
#' - dracodoc (<https://gist.github.com/dracodoc>)
#'
#' @section Source:
#' - <https://stackoverflow.com/a/28845828/5578429>
#' - <https://gist.github.com/dracodoc/74e5d2042efec0dfd9fcdbe6d65cf7e2>
#'
#' @md
clippy <- function(x = NULL) {
    # Get the data to copy
    if (is.null(x)) {
        result <- .Last.value
    }

    result <- eval(quote(x))

    # Apparently this provides multi-platform access for writing to the clipboard?
    clipboard <- file(description = "clipboard")

    # Copy the stuff to the clipboard in a type-sensitive way
    if ("data.frame" %in% class(result)) {
        utils::write.table(result, clipboard, sep = "\t",
                           quote = FALSE,
                           row.names = FALSE,
                           col.names = TRUE)
    } else {
        # It's assumed to be a simple vector
        utils::write.table(result, clipboard, sep = "\t",
                           quote = FALSE,
                           row.names = FALSE,
                           col.names = FALSE)
    }

    # As a confirmatory step, return the contents of the clipboard. If you have
    # .Last.value displayed in RStudio's 'Environment' tab, you'll see it change.
    # From dracodoc's generic clipboard funs:
    # https://gist.github.com/dracodoc/74e5d2042efec0dfd9fcdbe6d65cf7e2
    os <- Sys.info()[['sysname']]

    if (os == "Windows") {
        contents <- utils::readClipboard()
    } else if (os == "Darwin") {
        pb_read_lines <- function() {
            clip_r_mac <- pipe("pbpaste")
            lines <- readLines(clip_r_mac)
            close(clip_r_mac)
            return(lines)
        }

        contents <- pb_read_lines()
    }

    return(invisible(contents))
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
#' @md
coinflip <- function(n = 1) {
    sample(c(TRUE, FALSE), size = n, replace = TRUE)
}

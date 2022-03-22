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
        x <- .Last.value
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
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
coinflip <- function(n = 1) {
    sample(c(TRUE, FALSE), size = n, replace = TRUE)
}



#' Return a value or expression if something is NA
#'
#' This is just shorthand for `ifelse(is.na(x), TRUE, FALSE)` because I like using
#' that pattern in my `dplyr` pipelines.
#'
#' @param x (Vector) A vector to test.
#' @param yes (Any) The value to return if `x` is `NA`.
#' @param no (Any) The value to return if `x` is **not** `NA`.
#'    Set this to `NULL` (its default setting) to return `x`.
#'
#' @return The object in `yes` or `no`, depending on the outcome.
#' @export
#'
#' @examples
#' vec <- c("hello", NA, "hi")
#'
#' if_na(vec, "REPLACED")
#' #> [1] "hello"    "REPLACED" "hi"
#'
#' if_na(vec, "Was NA", "Was not NA")
#' #> [1] "Was not NA" "Was NA"     "Was not NA"
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
if_na <- function(x, yes = TRUE, no = NULL) {
    # Keeping x by default means this can work like a simple tidyr::replace_na().
    if (is.null(no)) no <- x

    ifelse(is.na(x), yes, no)
}



#' Generate random seeds to preview their effects
#'
#' This function will throw an error if you try to generate a new seed non-interactively 
#' (e.g. in an Rmarkdown document that is being knitted, or an R script that is being 
#' executed from the command line). This is because `try.seed()` changes the random seed, 
#' and that could affect the rest of your script in ways that you don't want. It is 
#' allowed to run non-interactively if you set the `seed` argument to a non-`NULL` number.
#'
#' This function picks a random seed, announces what that seed is, and then uses it to
#' evaluate an expression. For example, if you are creating a network graph whose layout
#' is calculated from randomly-chosen starting positions, `try.seed()` lets you run that
#' plotting function over and over with a new seed each time, until you find a layout that
#' you would like to keep. 
#' 
#' When you find a seed that you want to keep, you should copy it from the console and 
#' provide it as the `seed` argument.
#'
#' @param expr (Expression) An expression.
#' @param seed (Integer or `NULL`) If `NULL`, generate and set a new seed. If you provide
#'    a seed in this argument, that seed will be used and no new seed will be generated.
#'
#' @return The evaluated `expr`.
#' @export
#'
#' @examples
#' \dontrun{
#' try.seed(runif(5))
#' }
#'
#' #> Seed is: 1605125467
#' #> [1] 0.2582169 0.9739978 0.4126912 0.1326866 0.1336819
#' #>
#' 
#' \dontrun{
#' try.seed(runif(5),
#'          seed = 1605125467)  # The announced seed
#' }
#'
#' #> [1] 0.2582169 0.9739978 0.4126912 0.1326866 0.1336819
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
try.seed <- function(expr, seed = NULL) {
    if (is.null(seed)) {
        if (interactive() == FALSE) {
            stop("try.seed() is not allowed to generate new seeds in a script. See '?try.seed'.")
        }
        
        seed <- sample(0:.Machine$integer.max, 1)
        message("Trying seed: ", seed)
    }
    
    set.seed(seed)
    
    eval(expr)
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



#' Pipeline- and knit-friendly `View()`
#'
#' This function can be safely inserted into a pipeline while it is being developed.
#' It will run `View()` on the object only if R is running interactively. It returns
#' its input so that the pipeline can continue running.
#'
#' @param x (Object) An object to `View()`.
#'
#' @return Runs `View()` on the object if R is running interactively. Finally, 
#'     invisibly returns `x`.
#' @export
#'
#' @examples
#' \dontrun{
#' Show(iris)
#' }
#' 
#' @md
Show <- function(x) {
    if (interactive() == TRUE) {
        utils::View(x, deparse(substitute(x)))
    }
        
    invisible(return(x))
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

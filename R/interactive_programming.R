# Functions for interactive programming



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



#' Generate random seeds to preview their effects
#'
#' This function picks a random seed, announces what that seed is, and then uses it to
#' evaluate an expression. For example, if you are creating a network graph whose layout
#' is calculated from randomly-chosen starting positions, `try.seed()` lets you run that
#' plotting function over and over with a new seed each time, until you find a layout that
#' you would like to keep. 
#' 
#' This function will throw an error if you try to generate a new seed non-interactively 
#' (e.g. in an Rmarkdown document that is being knitted, or an R script that is being 
#' executed from the command line). This is because `try.seed()` changes the random seed, 
#' and that could affect the rest of your script in ways that you don't want. It is 
#' allowed to run non-interactively if you set the `seed` argument to a non-`NULL` number.
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
    
    expr
}

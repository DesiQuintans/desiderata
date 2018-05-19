# Extra functions that help me work with files.



#' Load an RDS file verbosely
#'
#' When you load a RDS file (serialised R objects e.g. a saved dataframe), this function
#' announces when the file was created so that you can see whether you are using an
#' up-to-date version. Often used in Rmarkdown documents.
#'
#' @param ... (Character) Arguments to send to `file.path()`. You can provide a complete
#'    path as a single string, or incrementally build a path with many strings.
#'
#' @return An imported RDS file.
#' @export
#'
#' @examples
#' data <- loadRDS("path", "to", "data.rds")
#'
#' #> Loading data.rds
#' #> It was compiled on 2018-05-16 11:36:05.
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
loadRDS <- function(...) {
    file <- file.path(...)

    cat("Loading ",
        basename(file),
        ".\nIt was compiled on ",
        as.character(file.info(file)$mtime),
        ".",
        sep = "")

    # cat() needs as.character() to display formatted dates.
    # https://www.rdocumentation.org/packages/base/versions/3.4.0/topics/cat

    return(readRDS(file))
}



#' Create a folder path
#'
#' Allows you to build a file path using `file.path()`, and then makes sure that it
#' actually exists by creating the folders in the path if needed.
#'
#' @param ... (Character) Arguments to send to `file.path()`. You can provide a complete
#'    path as a single string, or incrementally build a path with many strings.
#'
#' @return (Character) A file path. Automatically adds trailing slashes if required.
#' @export
#'
#' @examples
#' make_path("path", "subfolder")
#'
#' #> [1] "path/subfolder"
#'
#' # And the path/subfolder/ folders were also created in the working directory.
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
make_path <- function(...) {
    path <- file.path(...)

    if (grepl("\\.", basename(path)) == TRUE) {
        pathToBuild <- file.path(dirname(path), "/")
        # The basename has a file extension, which means that it ends with a filename.
        # Therefore dirname() is returning a folder path without the trailing slash.
        # Add the trailing slash or else dir.create() will not create the last folder.
    } else if (substr(path, nchar(path), nchar(path)) == "/") {
        pathToBuild <- path
        # The last character in the path is a slash, therefore this is a fully-qualified folder
        # path. I can create it as-is.
    } else {
        pathToBuild <- file.path(path, "/")
        # If path does not have a file extension and doesn't have a trailing slash, then it is
        # a folder path with no trailing slash -- but we can't use the above code because
        # dirname() cuts off the last folder in this case.
    }

    if (!dir.exists(pathToBuild))
        dir.create(pathToBuild, recursive = TRUE)

    return(path)
}

# Extra functions that help me work with files.



#' Load an RDS file and announce when it was created
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
#' # data <- loadRDS("path", "to", "data.rds")
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



#' Build a path, creating subfolders if needed
#'
#' Whereas `base::file.path()` only concatenates strings to build a path, `make_path()`
#' *also* makes sure those folders exist.
#'
#' @param ... (Character) Arguments to send to `file.path()`. You can provide a complete
#'    path as a single string, or incrementally build a path with many strings.
#'
#' @return (Character) A file path. Automatically adds trailing slashes if required.
#' @export
#'
#' @examples
#' # make_path("path", "to", "subfolder")
#'
#' #> [1] "path/to/subfolder"
#' # And the path/to/subfolder/ folders were also created in the working directory.
#'
#' # saveRDS(iris, make_path("subfolders/to/compiled/data/iris.rds"))
#'
#' # Creates all of the subfolders required for writing iris.rds.
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

#' Apply a function to every file in a folder that matches a regex pattern
#'
#' Create a list of files that match a regex search pattern, and then apply a function to
#' each file. For example, run `read_csv` on every .csv file in a folder.
#'
#' @param path (Character) The path to the folder.
#' @param pattern (Character) A regular expression search pattern.
#' @param func (Name) The bare name of a function to execute on each file.
#' @param ...  (...) Optional arguments that will be passed to `func`.
#' @param recursive (Logical) If `TRUE`, also search inside the subfolders of `path`.
#' @param ignorecase (Logical) If `TRUE`, `pattern` is case-insensitive.
#'
#' @return Invisibly returns a single dataframe with all of the input files row-binded
#'    together. A new column, `orig_source_file`, contains the source file's name.
#' @export
#'
#' @examples
#'
#' # rain <- apply_to_files(path = "Raw data/Rainfall", pattern = "csv",
#' #                        func = readr::read_csv, col_types = "Tiic",
#' #                        recursive = FALSE, ignorecase = TRUE)
#'
#' # dplyr::sample_n(rain, 5)
#'
#' #> # A tibble: 5 x 5
#' #>
#' #>   orig_source_file       Time                 Tips    mV Event
#' #>   <chr>                  <dttm>              <int> <int> <chr>
#' #> 1 BOW-BM-2016-01-15.csv  2015-12-17 03:58:00     0  4047 Normal
#' #> 2 BOW-BM-2016-01-15.csv  2016-01-03 00:27:00     2  3962 Normal
#' #> 3 BOW-BM-2016-01-15.csv  2015-11-27 12:06:00     0  4262 Normal
#' #> 4 BIL-BPA-2018-01-24.csv 2015-11-15 10:00:00     0  4378 Normal
#' #> 5 BOW-BM-2016-08-05.csv  2016-04-13 19:00:00     0  4447 Normal
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' - Gregor (<https://stackoverflow.com/users/903061/gregor>)
#'
#' @section Source:
#' <http://stackoverflow.com/a/24376207>
#'
#' @md
apply_to_files <- function(path, pattern, func, ..., recursive = FALSE, ignorecase = TRUE) {
    file_list <- list.files(path = path,
                            pattern = pattern,
                            full.names = TRUE,  # Return full relative path.
                            recursive = recursive,  # Search into subfolders.
                            ignore.case = ignorecase)

    df_list <- lapply(file_list, func, ...)

    names(df_list) <- basename(file_list)
    out <- dplyr::bind_rows(df_list, .id = "orig_source_file")

    return(invisible(out))
}

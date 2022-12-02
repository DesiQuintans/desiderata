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
        ".\n",
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
    path <- stringr::str_replace_all(path, "/+", "/")
    path <- stringr::str_replace_all(path, stringr::fixed("/."), ".")

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
#' @param method (Character) The method to use to merge all of the files into on
#'   dataframe. `"full_join"` (the default) returns all columns and rows. `"left_join"`
#'   returns all rows from the first file, and all columns from subsequent files.
#'   `"inner_join"` returns rows from the first file that have matches in subsequent files.
#'   `"row_bind"` simply appends each file to the end of the last.
#'
#'
#'   appends each new file to last row of the dataframe, but leaves `NA`s when the files
#'   contain different columns.
#'
#' @return Invisibly returns a single dataframe with all of the input files merged
#'   together. If `method = "row_bind",` then a new column, `orig_source_file`, contains
#'   the source file's name. The "join" methods do not have this column because the values
#'   are mixed together.
#' @export
#'
#' @examples
#'
#' # rain <- apply_to_files(path = "Raw data/Rainfall", pattern = "csv",
#' #                        func = readr::read_csv, col_types = "Tiic",
#' #                        recursive = FALSE, ignorecase = TRUE,
#' #                        method = "row_bind")
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
apply_to_files <- function(path, pattern, func, ..., recursive = FALSE, ignorecase = TRUE, method = "full_join") {
    file_list <- list.files(path = path,
                            pattern = pattern,
                            full.names = TRUE,      # Return full relative path.
                            recursive = recursive,  # Search into subfolders.
                            ignore.case = ignorecase)

    df_list <- lapply(file_list, func, ...)

    # The .id arg of bind_rows() uses the names to create the ID column.
    names(df_list) <- basename(file_list)

    out <- switch(method,
                  "full_join"  = plyr::join_all(df_list, type = "full"),
                  "left_join"  = plyr::join_all(df_list, type = "left"),
                  "inner_join" = plyr::join_all(df_list, type = "inner"),
                  # The fancy joins don't have orig_source_file because the values were
                  # getting all mixed together.
                  "row_bind"   = dplyr::bind_rows(df_list, .id = "orig_source_file"))

    return(invisible(out))
}



#' Write a dataframe to a CSV and RDS
#'
#' @param df (Dataframe) A dataframe. 
#' @param path (Character) The output folder. It will be created if it does not 
#'    exist. Two sub-directories (`/csv/` and `/rds/`) will also be created if 
#'    they don't exist.
#' @param basename (Character) The name of the resulting files, without a 
#'    path or extension. 
#'
#' @return Returns `TRUE` invisibly on success.
#' @export
#'
#' @examples
#' # write_csv_rds(iris, "_output", "my_iris")
#' 
#' #> Wrote 'iris' to '_output/csv/my_iris.csv'.
#' #> Wrote 'iris' to '_output/rds/my_iris.rds'.
#' 
#' @md
write_csv_rds <- function(df, path, basename) {
    df_name <- substitute(df)
    
    if (is.data.frame(df) == FALSE) {
        stop("is.data.frame(", df_name, ") is not TRUE.")
    }
    
    csv_file <- make_path(path, "/csv/", basename, ".csv")
    rds_file <- make_path(path, "/rds/", basename, ".rds")
    
    readr::write_csv(df, csv_file)
    readr::write_rds(df, rds_file)
    
    csv_exists <- file.exists(csv_file)
    rds_exists <- file.exists(rds_file)
    
    if (csv_exists & rds_exists) {
        message("Wrote '", df_name, "' to '", csv_file, "'.")
        message("Wrote '", df_name, "' to '", rds_file, "'.")
        
        return(invisible(TRUE))
    } else {
        csv_error <- ifelse(csv_exists == FALSE, 
                            paste0("'", csv_file, "'"),
                            character(0))
        
        rds_error <- ifelse(rds_exists == FALSE, 
                            paste0("'", rds_file, "'"), 
                            character(0))
        
        bad_files <- paste0(c(csv_error, rds_error), collapse = " and ")
        
        stop("Failed to write ", bad_files, ".")
    }
}



#' Save a ggplot as an A4 image
#' 
#' The dimensions are for the pixel size of an A4 page at 300 DPI.
#' This is appropriate for inserting into Word without much detail loss, and 
#' with appropriate text and element sizes.
#'
#' @param path (Character) The output folder. It will be created if it does not 
#'    exist. One sub-directory (`/png/`) will also be created if it doesn't exist.
#' @param basename (Character) Basename of the image, without path or extension. 
#'    Always saved as a `.png`.
#' @param plot (ggplot) The plot object to save. By default, saves the most 
#'    recent ggplot.
#' @param portrait (Logical) `FALSE` by default, which outputs in landscape. 
#'    If `TRUE`, flips the page dimensions to output in portrait.
#'
#' @return Returns `TRUE` invisibly if successful.
#' @export
#' 
#' @examples
#' # qplot(mpg, wt, data = mtcars)
#' # save_a4("_test", "x_mpg y_wt")
#' 
#' #> Wrote 'ggplot2::last_plot' to '_test/png/x_mpg y_wt.png'.
#' 
#' @md
save_a4 <- function(path, basename, plot = ggplot2::last_plot(), portrait = FALSE) {
    png_path <- make_path(path, "/png/", basename, ".png")
    png_path <- stringr::str_replace(png_path, 
                                     stringr::fixed(".png.png"),
                                     ".png")
    
    if (portrait) {
        h <- 3508
        w <- 2481
    } else {
        h <- 2481
        w <- 3508
    }
    
    ggplot2::ggsave(png_path, plot = plot,
           height = h, width = w, units = "px", dpi = 300)
    
    if (file.exists(png_path)) {
        message("Wrote '", substitute(plot), "' to '", png_path, "'.")
        
        return(invisible(TRUE))
    } else {
        stop("Failed to write '", png_path, "'.")
    }
}

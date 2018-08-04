# desiderata 0.16.0 (2018-08-05)

- ADD - `method` arg to `apply_to_files()` to provide more options than simple row-binding. 
- MOD - Started recording dates of changes.

# desiderata 0.15.0

- ADD - `clippy()`, which copies dataframes, vectors, and the results of expressions to the system clipboard. Tested on Windows, but hopefully also works on Mac!
- MOD - Added `ties` arg to `Mode()`. If `ties == FALSE` and there are multiple modes (e.g. `c(2, 2, 1, 1)`), only the first mode (`2`) will be returned.

# desiderata 0.14.0

- ADD - `regex` args to `drop_empty_rows()` and `drop_empty_cols()`.

# desiderata 0.13.0

- ADD - `drop_empty_rows()`, which deletes empty rows from a dataframe. A row is empty if every cell is `NA`, `NULL`, `""`, or `0`. You can select which columns to use or omit when making this empty/not-empty decision. For example, columns containing IDs or names will probably never be empty and should be ignored.
- ADD - `collapse_df()`, which collapses every cell of a dataframe (or a subset of one) into a vector. Useful for grabbing every number in a table and plotting it on a histogram, for example.

# desiderata 0.12.0

- ADD - `drop_empty_cols()`, which deletes empty columns from a dataframe. A column is empty if every row is `NA`, `NULL`, `""`, or `0`.

# desiderata 0.11.0

- REM - Title alignment settings in `theme_desi_base()`.
- ADD - `align_titles()` to horizontally align the title and subtitle of a ggplot.
- FIX - `overwrite_df()` no longer returns the overwritten columns as factors.
- FIX - `round_to_places()` no longer rounds numbers twice. `round_to_places(16.666667, 2)` used to return `17`, but it now correctly returns `16.67`.

# desiderata 0.10.0

- ADD - `vec_to_regex()`, which collapses vectors into a regular expression.
- ADD - `basic_colour_names`, a built-in dataset that contains the names of 197 browser-compatible web colours.
- FIX - `set_seed_any()` properly checks if the `digest` package is installed.

# desiderata 0.9.0

- ADD - `cat_wrap()`, which prints text to the console while wrapping the output.

# desiderata 0.8.0

- ADD - `percentile()`, which is an alias of `stats::quantile()` with some useful default percentiles defined.
- ADD - `set.seed.any()` deprecated; use `set_seed_any()` instead.
- MOD - Moved package `digest` from 'Imports' to 'Suggests'. It is only used once by `set_seed_any()`.

# desiderata 0.7.0

- MOD - Some changes to `theme_desi_base()`.
- ADD - `rotate_x_text()` and `rotate_y_text()` for rotating the tick labels of ggplot2 plots. Comes with sane default settings.

# desiderata 0.6.0

- ADD - `%notin%` which is just `!(x %in% y)` in a more readable form.
- ADD - `%pctin%` which returns the percent of `x` that appears in `y`.

# desiderata 0.5.0

- ADD - `apply_to_files()`, which applies a function to a list of files that matched a regex search pattern. Use it to import all spreadsheets in a folder, for example. Includes recursive searching.

# desiderata 0.4.0

- ADD - `overwrite_df()`, which lets you use regex to match and replace across all cells in a dataframe. This is incredibly convenient as the final step before printing a table in your Rmarkdown document, because you can blank out NAs and other irrelevant values to avoid distracting the reader. Not intended for use with actual data tidying ;use `dplyr::recode()` or similar for that.

# desiderata 0.3.0

- ADD - `NEWS.md` file to track changes to the package.
- ADD - `shush()` lets you run any expression without allowing it to print `cat()`, `print()`, `warning()` or `message()` to the console. Useful for running functions that have `cat()` hard-coded into them.
- FIX - Function examples that alter libraries or have significant side-effects (creating plots, messing with the filesystem) have been commented so that they do not run with `example()` or `R CMD CHECK`.

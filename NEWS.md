# desiderata 0.10.0

- ADD - `vec_to_regex()`, which collapses vectors into a regular expression.
- ADD - `basic_colour_names`, a built-in dataset that contains the names of 197 browser-compatible web colours. 

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

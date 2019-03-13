# desiderata 0.32.0 (2019-03-07)

- ADD - `plot_arrange()` arranges base R plots into a grid. It's like `gridExtra::grid.arrange()`, but for base R plots.
- ADD - `nth_word()` which grabs the nth part of a string that is delimited by a regular expression.
- MOD - `Mode()` has a new argument value, `break_ties = "NA"`. If more than one mode is found in the input, it will return `NA` instead of having to choose between them.



# desiderata 0.31.0 (2019-03-04)

- ADD - `palette_distant()` which has 48 colours that are not located adjacent to each other along the RGB and HSV codings.
- ADD - `top_tail()` which retrieves first and last rows from a dataframe.
- ADD - `try.seed()` for running an expression with a new random generator seed each time. The seed is announced in the console so that when you find one that you like, you can copy it use it in `set.seed()` in your script.



# desiderata 0.30.1 (2019-03-03)

- FIX - `show_colours()` now correctly handles lists of colours shorter than 4.



# desiderata 0.30.0 (2019-02-26)

- MOD - Changed the interface of `Mode()` so that it is simpler to use and has more tie-breaking options. Old args `ties` and `mean` are deprecated and will throw warnings, but I have done my best to maintain their functionality so that this is not a breaking change.



# desiderata 0.29.0 (2019-02-17)

- ADD - `howmany()` is now a generic function that includes methods for dataframes, tables, and vectors.
- REM - `howmany_df()` is defunct. Now, just pass the dataframe to `howmany()` and it will choose the right method.
- REM - `round_to_places()` is defunct.



# desiderata 0.28.0 (2019-02-13)

- ADD - `common_stem()` for comparing several strings and returning the 'stem' (a range of characters from the first position to the first mismatch) that is common to all of them.
- ADD - `str_rev()` for mirroring every string in a vector of strings.
- MOD - `mirror_matrix()` now supports mirroring of both column order and row order.
- FIX - `mirror_matrix()` now runs 45 times faster (!).
- MOD - `join` argument added to `uw()`. Gives you control over how separate hard-wrapped lines should be joined together.



# desiderata 0.27.0 (2019-02-07)

- ADD - `useNA` argument to `count_unique()` so that NAs are also shown in the table.
- FIX - `clippy()` correctly copies `.Last.value` to the clipboard if no `x` argument is provided to it.



# desiderata 0.26.0 (2019-02-01)

- ADD - `howmany_df()` for quickly summarising the number of unique values in every column of a dataframe.
- FIX - `percentile()` help file documents `na.rm` argument now.
- FIX - Forgot to specify the packages for some non-base functions.
- FIX - The example long-lines in `uw()` documentation were too long.



# desiderata 0.25.0 (2019-01-25)

- ADD - `quick_lm()` for fast data exploration using plots of `x ~ y` linear models.



# desiderata 0.24.0 (2019-01-07)

- ADD - `split_n()` divides a vector into groups by assigning each entry a grouping number.



# desiderata 0.23.6 (2018-12-28)

- ADD - `drop_invar_cols()` drops columns whose values are all the same (for character/factor), or whose values are very close together (for numeric).
- MOD - Edited the documentation for the `drop_empty_()` functions to make it clearer that `df` is being subset with the `to/from/cols` arguments. 
- MOD - `percentile()` given an `na.rm` argument.
- MOD - `show_colours()` given a `pad` argument for padding out the table when the number of colours doesn't perfectly fit a square. By default this is white (because the default plot background is white).


# desiderata 0.23.5 (2018-11-22)

- ADD - `uw()` ("unwrap") takes hard-wrapped strings, removes the cosmetic linebreaks and indentation, and outputs it as a single combined string.



# desiderata 0.23.4 (2018-11-20)

- REM - `round_to_places()` is deprecated. Use the base function `round()` with a `digits` argument instead (e.g. `round(n, digits = 2)`).
- FIX - Rmd Analysis Document template no longer defaults to `cache = TRUE` because it can sometimes cause annoying bugs.



# desiderata 0.23.3 (2018-11-15)

- MOD - `percentile()` now passes `...` to `plot()` if the `plot` argument is `TRUE`.
- MOD - `percentile(plot = TRUE)` now shows values on the plotted points.
- MOD - `build_palette()` is now a public function so that you can get the functionality of the `palette_...()` family in your own colour lists.
- ADD - `spaced` arg to `palette_...()` and `build_palette()` functions. This argument lets you sample `n` colours evenly across the colour list. If you have a list of colours that are sorted by hue, for example, this helps you pick colours that are further away from each other.
- ADD - `rcols_as_hex()` converts built-in R colours to hex values (e.g. "goldenrod" → "#DAA520").
- ADD - `palette_builtin()` lets you access the list of colours provided by `colours(distinct = TRUE)`, adding features like transparency and random colour selection via the `build_palette()` framework.
- MOD - Documentation for all of the `palette_...()` functions is now bundled together with `build_palette()`.
- MOD - The librarian auto-installer in the 'Analysis Document' Rmd template now works faster.



# desiderata 0.23.2 (2018-11-07)

- MOD - Added a chunk to the 'Analysis Document' Rmd template that installs my package-management package 'librarian' if it does not already exist. Librarian makes it very easy to maintain a package list in your analysis document, automatically installs packages from CRAN/Bioconductor/GitHub if they're not already installed, and lets your analysis document set itself up on any other computer. <https://github.com/DesiQuintans/librarian>



# desiderata 0.23.1 (2018-11-05)

- ADD - `plot` argument for `percentile()` creates a graphical representation of the percentile values.



# desiderata 0.23.0 (2018-11-02)

- ADD - `coinflip()` which randomly returns `TRUE` or `FALSE`.
- ADD - `collapse_vec()` which lets you concatenate the elements of multiple vectors into one long string.
- ADD - `count_unique()` which counts how many times each unique element in a vector is repeated.



# desiderata 0.22.0 (2018-10-19)

- MOD - `howmany()` now accepts multiple vectors with `...` instead of only one vector.
- MOD - Various changes to the Analysis Document Rmarkdown template.
- FIX - Add all new functions to the readme.
- ADD - Starting from this version, my other package `librarian` is also loaded.



# desiderata 0.21.0 (2018-10-15)

- ADD - Rmarkdown template for the analysis documents that I write.



# desiderata 0.20.0 (2018-10-14)

This update is mostly about adding more plotting tools that I needed while I was making network graphs in `igraph`.

- ADD - `palette_mrmrs()` which contains 16 web-safe colours from [Adam Morse](https://clrs.cc/).
- ADD - `palette_picked()` which contains 14 colours that I picked as high-contrast replacements from `palette_distinct()`. Nearly all of them are similar to the ones from `palette_mrmrs()`, that's pretty neat!
- ADD - `show_colours()` takes a vector of colours and shows them in a nice plot.
- ADD - `is.prime()` checks if a number is prime or not.
- ADD - `mirror_matrix()` mirrors a matrix horizontally.
- ADD - `alpha` argument to `palette_distinct()`. Applies a constant transparency to all of the colours that the function returns, which is useful for generating colours in graphs that are heavily overplotted.
- ADD - A private function `desiderata:::find_dims()` that finds the dimensions of a grid that will fit a certain number of cells. Used internally for `show_colours()`.
- ADD - A private function `desiderata:::build_palette()` 
- REM - `set.seed.any()` was deprecated for months and is now removed. Use `set_seed_any()` instead.



# desiderata 0.19.0 (2018-10-13)

- ADD - `howmany()` is an alias for `length(unique(x))`.
- FIX - `shush()` no longer causes invisibly-returned output to print.



# desiderata 0.18.0 (2018-10-09)

- MOD - `desi_theme_base()` doesn't remove the grid lines anymore.
- FIX - `palette_distinct()` returns the requested number of colours when `random = TRUE`.



# desiderata 0.17.0 (2018-08-09)

- ADD - `se_mean()`, which calculates the standard error of the mean.



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

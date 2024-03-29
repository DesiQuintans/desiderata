# Unreleased

- REM - `sift()` is removed. It is now a completely separate package, <https://github.com/DesiQuintans/sift>.
- ADD - `material2014_colblind` --- A new datasets containing the Material Design 2014 colour palette, plus simulated colourblind conversions via `khroma:::anomalize()`.
- ADD - `chunk_int()`, which tells you how an integer should be divided into `n` chunks (e.g. chunk 1 gets rows 1-10, chunk 2 gets rows 11-20, etc.).
- MOD - Changed R dependency to >= 3.6.0 instead of demanding >= 4.2.0. Not everyone wants to update so often!
- MOD - Removed all version requirements for dependencies.
- MOD - `na_in_row()` produces two new output columns: `notna_in_row_count` and `notna_in_row_prop`.
- MOD - `show_colours()` has new `n` argument to control how many rows the resulting grid will have.
- MOD - Private function `find_dims()` takes new argument `x` to support the above change in grid sizing.
- MOD - `show_colours()` now fills the grid by column (top to bottom, then left to right) instead of by row (left to right, then top to bottom). This makes more pleasing colour palette grids IMO.
- FIX - Removed redundant `eval()` from `try.seed()`.


# desiderata 0.43.0 (2022-11-28)

- MOD - `drop_empty_cols()` and `drop_empty_rows()` are no longer hard-coded to consider the value `0` as something that counts as 'empty'. This means that columns and rows that are full of `0` are kept, since zeroes can be meaningful. If you want to consider `0` as empty, use the argument `regex = "^0$"` to match it.
- MOD - `drop_empty_cols()` and `drop_empty_rows()` can now report their changes with the `report` argument
- ADD - `na_in_row()` finds the count and proportion of `NA` for each row of a dataframe, using tidyselectors to choose which columns to look at.


# desiderata 0.42.0 (2022-10-29)

- ADD - `keep_every()`, which uses a string to control which elements of a vector are kept or removed. For example, `"k-"` keeps odd elements, `"-k"` keeps even elements, and `"-kk--"` keeps the 2nd and 3rd element out of every 5.
- ADD - `round_to_duration()`, which _roughly_ converts a time duration (e.g. 37 days) into a different unit of time (e.g. 1.215606 months), with optional rounding to a nearest value (e.g. 1.25 months).
- ADD - `consecutive_week()`, which counts the number of 7-day weeks between two dates **OR** the number of calendar weeks starting on Mondays (AKA isoweeks) between two dates.
- ADD - `not.na()`, a more noticeable equivalent of `!is.na()`.
- ADD - `not.nan()`, a more noticeable equivalent of `!is.nan()`.
- ADD - `write_df()`, which writes a dataframe to a .csv and .rds in one shot.
- ADD - `save_a4()`, which saves a ggplot to a .png, with defaults suitable for a full A4 page in landscape.
- MOD - The X axis limits of `percentile()`'s plots are no longer hard-coded to 0-100 %.
- MOD - `round_to_nearest()` now accepts `dir = "both"` to be more similar to other functions with a `dir` argument, whose names I can't remember right now.
- FIX - `desi_base_theme()` removes the grid lines now, as the description always said.
- MOD - `desi_base_theme()` now has a `...` argument for sending further arguments to `ggplot2::theme()`.
- FIX - `make_path()` now condenses multiple directory separators (`/`) into one, and also respects file extensions (i.e. `make_path("dir", "file", ".rds")` will output `dir/file.rds` and **not** `dir/file/.rds`).
- MOD - Added `glue` and `readr` to dependencies.



# desiderata 0.41.0 (2022-05-11)

- ADD - `split_size()` to split a vector into chunks of known size (or smaller).



# desiderata 0.40.0 (2022-03-22)

- ADD - `ensure()` is for quickly testing your code, returning an Error if a test evaluates `FALSE`. For more extensive testing needs, look at the `assertr` package.
- ADD - `assign_groups()` assigns groups to elements of a vector, with good handling for duplicates.
- REM - `split_n()` is deprecated because its output was not intuitive when it came to unsorted or duplicated input. Use `assign_groups()` instead.



# desiderata 0.39.1 (2021-07-06)

- FIX - `drop_invar_cols()` now recognises all column types, including logical columns and list columns.


# desiderata 0.39.0 (2021-07-05)

- ADD - `uw0()` unwraps hard-wrapped lines without introducing spaces between the lines. This is a shortcut for `uw(..., collapse = "", join = "")`.


# desiderata 0.38.0 (2020-06-25)

- ADD - `rows_with_na()` only keeps rows of a dataframe that contain at least 1 `NA`.


# desiderata 0.37.0 (2020-04-04)

- MOD - `show_colours()` argument `arrange`, which lets you arrange the colours as a rectangular panel (by default), or as horizontal or vertical stripes.


# desiderata 0.36.0 (2019-12-20)

- ADD - `Show()` is a version of `View()` that can be used inside pipelines and Markdown documents.
- ADD - `triangle_num()` calculates the _n_th Triangle Number, which is like factorials but with addition. For example: T3 = 1 + 2 + 3 = 6.



# desiderata 0.35.0 (2019-12-11)

- ADD - `IQR_outliers()` marks elements of a vector that are outliers according to the 1.5 * IQR rule.
- ADD - `encode_signif()` turns p-values into significance codes (e.g. 0.05 → *).
- ADD - `unique_n()` keeps the first `n` unique elements in a vector.
- MOD - Rearranged the README.



# desiderata 0.34.1 (2019-11-05)

- MOD - `percentile()` argument `cuts` has two new default levels (0.025 and 0.9975) for easy observation of possible 95 % CI cut-offs.


# desiderata 0.34.0 (2019-10-22)

- ADD - `Mode()` has new accepted values for the `break_ties` argument: `median`, `median l`, and `median r`. These return the median of all of the modes, or in the case of an even number of modes (e.g. `c(1, 2, 3, 4)`), the mode to the left or right of the median (e.g. `2` or `3`).



# desiderata 0.33.0 (2019-07-15)

- FIX - `show_colors()` is now correctly exported and documented.
- ADD - `data(random_integers)` is a vector of 10,000 random integers between 0 and 100, generated by <https://www.random.org>. It is for specific applications like feature selection in machine learning, where adding a column of "true-random" numbers as a feature lets you draw a clear line between helpful and unhelpful features.
- ADD - `na_rm()` which wraps `stats::na.omit()` and hides its annoying printing side-effects when applied to a vector.
- ADD - `add_group_size()` adds the size of a grouped dataframe's groups (i.e. `dplyr::n()` under `dplyr::group_by()`) to the dataframe as a new column. Useful for complicated filtering operations such as "Only keep a genus if it has at least 3 species, and each species occurs in at least 4 sites within each region."
- ADD - `dots_char()` takes `...` and returns its elements as a character vector (or a string).
- ADD - `rev_sentence()` reverses the order of whole words in a string.
- REM - `howmany_df()` fully removed.
- REM - `round_to_places()` fully removed.



# desiderata 0.32.1 (2019-03-18)

- FIX - `Mode()` with `break_ties = "no"` explicitly set now returns the correct result.



# desiderata 0.32.0 (2019-03-07)

- ADD - `plot_arrange()` arranges base R plots into a grid. It's like `gridExtra::grid.arrange()`, but for base R plots.
- ADD - `nth_word()` which grabs the nth part of a string that is delimited by a regular expression.
- MOD - `try.seed()` has a `seed` argument that lets you directly set a seed there, instead of having to replace the whole function with the base `set.seed()` when you're done using it.
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

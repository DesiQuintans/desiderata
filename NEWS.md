# desiderata 0.3.0

- ADD - `NEWS.md` file to track changes to the package.
- ADD - `shush()` lets you run any expression without allowing it to print `cat()`, `print()`, `warning()` or `message()` to the console. Useful for running functions that have `cat()` hard-coded into them.
- FIX - Function examples that alter libraries or have significant side-effects (creating plots, messing with the filesystem) have been commented so that they do not run with `example()` or `R CMD CHECK`.

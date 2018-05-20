# desiderata

## Project participants

-   Desi Quintans (<https://twitter.com/eco_desi>)

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

## What is `desiderata`?

Desiderata is an assorted bunch o' functions that I (Desi) have collected or written as I needed them. 

## Installation

``` r
install.packages("devtools")

devtools::install_github("DesiQuintans/desiderata")

library(desiderata)
```

------------------------------------------------------------------------------------------

## Functions included

- **Dataframe functions**
    - Find and mark the longest run of TRUEs in a boolean vector
    - Mark the location of the last maximum value (peak) in a vector
    
- **File system functions**
    - Load an RDS file and announce when it was created
    - Create a folder path
    
- **Number functions**
    - Find the mode(s) of a numeric/character/factor vector
    - Geometric mean of a vector
    - Round a number to a fixed decimal place length
    - Round numbers to the nearest "pretty" value
    - Seed the random number generator with a character string
    - Calculate degree-days
    - Normalise a matrix column-wise between 0 and 1
    - Normalise a whole matrix between 0 and 1
    - Concatenate numbers together
    
- **Plotting functions**
    - Desi's `ggplot2` minimal base theme
    - A palette of 1,022 visually-distinct colours
    
- **Datetime tools**
    - Find the current month number relative to a starting date

------------------------------------------------------------------------------------------

## Dataframe functions

### Find and mark the longest run of TRUEs in a boolean vector

``` r
x <- c(T,    T,    F,    F,    F,    F,    F,     T,    T,    T,    T,    T,    T,   F)
mark_longest_run(x)

#> [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
```

### Mark the location of the last maximum value in a vector

``` r
input <- c(1, 2, 3, 3, 1)
mark_last_peak(input, threshold = NULL)

#> [1] FALSE FALSE FALSE  TRUE FALSE

mark_last_peak(input, threshold = 4)

#> [1] FALSE FALSE FALSE FALSE FALSE
```

## File system functions

### Load an RDS file and announce when it was created

``` r
data <- loadRDS("path", "to", "data.rds")

#> Loading data.rds
#> It was compiled on 2018-05-16 11:36:05.
```

### Create a folder path

``` r
make_path("path", "subfolder")

#> [1] "path/subfolder"

# And the path/subfolder/ folders were also created in the working directory.
```

## Number functions

### Find the mode(s) of a numeric/character/factor vector

``` r
vec <- c(1, 2, 3, 4, 4, 4, 3, 3, NA, NA, NA)

Mode(vec)
#> [1]  3  4 NA

Mode(vec, na.rm = TRUE)
#> [1] 3 4

Mode(vec, na.rm = FALSE, mean = TRUE)
#> [1] NA

Mode(vec, na.rm = TRUE, mean = TRUE)
#> [1] 3.5

Mode(1:4)
#> [1] 1 2 3 4

Mode(1:4, mean = TRUE)
#> [1] 2.5
```

### Geometric mean of a vector

``` r
vec <- c(1, 3, 9, 27, 81, 243, 729)

mean(vec)
#> [1] 156.1429

geomean(vec)
#> [1] 27
```
### Round a number to a fixed decimal place length

``` r
vec <- c(1.739006, 2, -1.4, 1.05, 1.90, 3.826)
rounded_vec <- round_to_places(vec, 2)

str(rounded_vec)
#> chr [1:6] "1.70" "2.00" "-1.40" "1.00" "1.90" "3.80"
```

### Round numbers to the nearest "pretty" value

``` r
vec <- c(1.739006, 2, -1.4, 1.05, 1.90, 3.826)
round_to_nearest(vec, 0.5)

#> [1]  1.5  2.0 -1.5  1.0  2.0  4.0
```

### Seed the random number generator with a character string

``` r
set.seed.any("Snake... Do you think love can bloom, even on a battlefield?")
```

### Calculate degree-days

``` r
degreedays(min = c(19, 20, 20, 21), 
           max = c(25, 24, 23, 22), 
           base = 22, 
           method = "modavg")

#> [1] 1.5 1.0 0.5 0.0
```

### Normalise a matrix column-wise between 0 and 1

``` r
normalize_colwise(matrix(1:12, ncol = 3))

#>      [,1]  [,2]      [,3]
#> [1,] 0.25 0.625 0.7500000
#> [2,] 0.50 0.750 0.8333333
#> [3,] 0.75 0.875 0.9166667
#> [4,] 1.00 1.000 1.0000000
```

### Normalise a whole matrix between 0 and 1

``` r
mat <- matrix(1:4, ncol = 2)

#>      [,1] [,2]
#> [1,]    1    3
#> [2,]    2    4

normalize_whole(mat, from_zero = TRUE)

#>      [,1] [,2]
#> [1,] 0.25 0.75
#> [2,] 0.50 1.00

normalize_whole(mat, from_zero = FALSE)

#>           [,1]      [,2]
#> [1,] 0.0000000 0.6666667
#> [2,] 0.3333333 1.0000000
```

### Concatenate numbers together

``` r
concat_nums(12, "76", NA, 1.5)

#> [1] 127601.5
```

## Plotting functions

### Desi's `ggplot2` minimal base theme

``` r
ggplot(iris, aes(x = Petal.Length, y = Sepal.Length)) + geom_point() + theme_desi_base()
```

![](images/theme_desi_base.png)

### A palette of 1,022 visually-distinct colours

``` r
# To see all of the colours (ordered left-to-right and top-to-bottom):
image(apply(matrix(1022:1, ncol = 73, nrow = 14, byrow = TRUE), 1, rev), col = palette_distinct())

# To get the first 4 colours:
palette_distinct(4)

#> [1] "#000000" "#FFFF00" "#1CE6FF" "#FF34FF"

# To pick 4 colours randomly:
palette_distinct(4, random = TRUE)

#> [1] "#2F2E2C" "#DFE3E6" "#5C424D" "#FFE47D"
```

![](images/palette_distinct.png)

## Datetime tools

### Find the current month number relative to a starting date

``` r
# These are the same thing.

consecutive_month(2015, "2016-02-04")
consecutive_month("2015-01-01", "2016-02-04")
#> [1] 14

consecutive_month("2015-02-01", "2016-02-04")
#> [1] 13
```

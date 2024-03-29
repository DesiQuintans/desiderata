# Functions for plots and stuff

#' My personally-preferred minimal base plot theme
#'
#' `theme_bw()` with no grid, no borders, legend at the top, and no legend title.
#'
#' @param ... (Character) Arguments to send to `ggplot2::theme()`.
#'
#' @return `ggplot2` theme objects.
#' @export
#'
#' @examples
#' # ggplot(iris, aes(x = Petal.Length, y = Sepal.Length)) +
#' #     geom_point() +
#' #     theme_desi_base()
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
theme_desi_base <- function(...) {
    return(
        ggplot2::theme_bw() +
        ggplot2::theme(panel.border    = ggplot2::element_blank(),  # No border around plot
                       legend.position = "top",                     # Legend at the top
                       legend.title    = ggplot2::element_blank(),  # No legend title
                       panel.grid      = element_blank()           # No grid lines
                      ) +
        # Other args to theme(). They're in a second theme() call so that they 
        # can overwrite previously-used values instead of producing an "already 
        # used" message.
        ggplot2::theme(...) 
    )
}



# Converting between colour representations -------------------------------

#' Convert R's built-in named colours to hex codes
#'
#' @param vec (Character) A character vector of R's built-in named colours. If left `NULL`,
#'    the entire contents of `grDevices::colours()` will be used.
#' @param distinct (Logical) Should only distinct colours be returned? Distinct colours do
#'    not share the same (0:255)^3 RGB space, and do not have the same hex code. Defaults
#'    to `FALSE` so that you don't get unexpectedly-short vectors.
#'
#' @return A named character vector of hex colours that were converted from R's built-in
#'    names. The names are the original R colour names that were provided in `vec`.
#' @export
#'
#' @examples
#' rcols_as_hex(c("tomato", "steelblue"))
#' #>    tomato steelblue
#' #> "#FF6347" "#4682B4"
#'
#' rcols_as_hex()
#' #>     white    aliceblue    antiquewhite    ... for all colours in colours()
#' #> "#FFFFFF"    "#F0F8FF"    "#FAEBD7"       ... for all colours in colours()
#'
#' rcols_as_hex(distinct = TRUE)
#' #>     white    aliceblue    antiquewhite    ... for all in colours(distinct = TRUE)
#' #> "#FFFFFF"    "#F0F8FF"    "#FAEBD7"       ... for all in colours(distinct = TRUE)
#'
#' rcols_as_hex(c("snow", "snow1"))
#' #>      snow     snow1
#' #> "#FFFAFA" "#FFFAFA"
#'
#' rcols_as_hex(c("snow", "snow1"), distinct = TRUE)
#' #>      snow
#' #> "#FFFAFA"
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
rcols_as_hex <- function(vec = NULL, distinct = FALSE) {
    vec_is_null <- is.null(vec)
    
    if (vec_is_null)
        vec <- grDevices::colours(distinct = distinct)
    
    if (distinct == TRUE & !vec_is_null) {  # !vec_is_null so this is not done twice to colours().
        vec <- vec[!duplicated(t(grDevices::col2rgb(vec)))]
    }
    
    # Keep the colour names by naming vec with itself.
    names(vec) <- vec
    
    p <- grDevices::col2rgb(vec, alpha = FALSE)  # Convert named R colours to RGB.
    hex <- grDevices::rgb(p["red",], p["green",], p["blue",],
                          names = colnames(p), maxColorValue = 255)  # RGB to Hex
    
    if (distinct == TRUE)
        hex <- hex[!duplicated(hex)]  # unique() does not keep the names of a vector.
    
    return(hex)
}



#' Convert Hex colours to HSV
#'
#' @param hexcol (Character) A vector of hex colours.
#' @param which  (Character) Which variables to return. `"HSV"` will return Hue,
#'    Saturation, and Value. Any combination of these letters will return those 
#'    columns, so `"HV"` will return only Hue and Value.
#'
#' @return A dataframe if more than one column is requested in `which`, 
#'    otherwise a numeric vector.
#' @export
#'
#' @examples
#' col2hsv(c("#FFD8B1", "#808000"))
#' 
#' #>          h         s         v
#' #> 0.08333333 0.3058824 1.0000000
#' #> 0.16666667 1.0000000 0.5019608
#' 
#' col2hsv(c("#FFD8B1", "#808000"), which = "hv")
#' 
#' #>          h         v
#' #> 0.08333333 1.0000000
#' #> 0.16666667 0.5019608
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' 
#' @md
col2hsv <- function(hexcol, which = "hsv") {
    hsv <- as.data.frame(t(grDevices::rgb2hsv(grDevices::col2rgb(hexcol))))
    
    columns <- unlist(strsplit(which, ""))
    
    return(hsv[, columns])
}



# Plotting functions ----------------------------------------------------------------

#' Preview a list of colours as a grid
#'
#' @aliases show_colors
#' 
#' @param col_list (Character) A vector of colours in RGB Hex or RGBA Hex format.
#' @param arrange (Character: `"rect"`, `"rows"`, or `"cols"`) By 
#'    default (`"rect"`), colours are displayed in a rectangular panel. `"rows"` 
#'    arranges them as horizontal stripes top-to-bottom, and `"cols"` arranges 
#'    them as vertical stripes left-to-right.
#' @param n (Integer) If not `NULL` (default), then controls how tall (in cells) 
#'    the resulting grid will be. You would typically do this if you were trying
#'    to show a palette that you have already manually padded to specific 
#'    dimensions, e.g. for a palette that has 14 tints/shades for each hue, use 
#'    `n = 14` to arrange all hues into columns of 14 cells.
#' @param pad (Character) If there are too few colours to fill all of the spaces in
#'    the grid, what colour should be used to pad it out? `pad = "last"` repeats the
#'    last colour in the colour list. A hex colour can be provided too. Otherwise,
#'    this defaults to white (`#FFFFFF``).
#' @param asp (Numeric or `NA`) The aspect ratio of the image. Tweak this if you
#'    want to produce a graphic with square cells. If `NA`, `plot.window`'s 
#'    default will be used.
#' @param main (Character or `NULL`) Leave this as `NULL` to generate a title. Supply a
#'    string to define your own title.
#'
#' @return A graphic that shows all of the colours in `col_list`.
#' @export
#'
#' @examples
#' show_colours(colours(distinct = TRUE))
#' 
#' show_colours(colours(distinct = TRUE), arrange = "cols")
#'
#' @md
show_colours <- function(col_list, arrange = "rect", n = NULL,
                         pad = "#FFFFFF", asp = NA, main = NULL) {
    list_name   <- deparse(substitute(col_list))
    orig_length <- length(col_list)

    # Build the matrix that is used by image(). image() rotates the matrix 90 degrees
    # anti-clockwise during filling, so the colours end up being placed in the wrong
    # order.

    if (arrange == "cols") {
        m <- matrix(1:orig_length, ncol = 1, nrow = orig_length, byrow = TRUE)
    } else if (arrange == "rows") {
        m <- matrix(orig_length:1, ncol = orig_length, nrow = 1, byrow = TRUE)
    } else {
        # In order to make a grid (as opposed to lines), image() must a 
        # rectangular matrix that has more than one cell in each row and column.

        if (orig_length < 4) {
            # Vectors < 4 can't be rectangular, so make them 2 x 2.
            grid <- find_dims(1:4, n)
        } else {
            grid <- find_dims(col_list, n)
        }
        
        new_length <- grid["x"] * grid["y"]
        
        # Pad the colour list to the necessary number of cells. Necessary because
        # the matrix function will raise an error if the dims are not multiples of
        # the vector's length.
        pad_string <- ifelse(pad == "last", col_list[orig_length], pad)
        
        col_list <- append(col_list, rep(pad_string, abs(new_length - orig_length)))
        
        m <- matrix(1:new_length, ncol = grid["x"], nrow = grid["y"], byrow = TRUE)
        m <- mirror_matrix(m)
    }
    

    # Make a plot title, if requested
    if (is.null(main) == TRUE) {
        plot_title <- paste0("Showing ", orig_length, " colours in '", list_name, "'")
    } else {
        plot_title <- main
    }

    # Plot the output.
    graphics::image(m,
                    col = col_list, axes = 0, cex.main = 1, asp = asp,
                    main = plot_title
    )
}



#' @rdname show_colours
#' @export
show_colors <- show_colours



#' Fit and plot a two-term linear model quickly
#'
#' @param formula (Formula) A two-term formula for both the linear model and the plot.
#' @param data (Dataframe) The dataframe to use for fitting and plotting.
#' @param ... (Args) Arguments that will be passed to `plot()`.
#'
#' @return A plot that shows a fit line and lists intercept, slope, and r-square,
#'    adjusted r-squared, etc. Invisibly returns the `lm` fit object so that it can
#'    be inspected.
#' @export
#'
#' @examples
#' quick_lm(Petal.Length ~ Sepal.Length, iris)
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @md
quick_lm <- function(formula, data, ...) {
    fit  <- stats::lm(formula = formula, data = data)
    summ <- summary(fit)
    
    intercept <- signif(stats::coef(fit)[[1]], 3)
    slope     <- signif(stats::coef(fit)[[2]], 3)
    rsq       <- signif(summ$r.squared, 3)
    adjrsq    <- signif(summ$adj.r.squared, 3)
    fstat     <- signif(summ$fstatistic[[1]], 3)
    dof       <- summ$fstatistic[[3]]
    
    # http://r.789695.n4.nabble.com/Extract-p-value-from-lm-for-the-whole-model-tp1470479p1470527.html
    pval      <- stats::pf(summ$fstatistic[1], summ$fstatistic[2], summ$fstatistic[3],
                    lower.tail=FALSE)
    pval      <- signif(pval, 4)
    
    graphics::plot(formula, data = data, ...)
    graphics::title(main = paste0("int = ", intercept,
                                  "    slope = ", slope,
                                  "    r^2 = ", rsq,
                                  "\n",
                                  "adj r^2 = ", adjrsq,
                                  "    f = ", fstat,
                                  "    dof = ", dof,
                                  "    p = ", pval))
    graphics::abline(fit)
    
    return(invisible(fit))
}



#' Arrange base R plots on a grid
#'
#' This is similar to `gridExtra::grid.arrange()`, but it works on base R
#' plots instead of ggplot objects.
#'
#' @param ... (Expressions) Functions that create plots.
#' @param nrow (Integer) Number of rows to arrange plots into.
#' @param ncol (Integer) Number of columns to arrange plots into.
#' @param par_args (List) A `list()` of arguments to pass into `par()`.
#'
#' @return Plots arranged in a grid.
#' @export
#'
#' @examples
#' plot_arrange(plot(Sepal.Length ~ Sepal.Width,  data = iris),
#'              plot(Petal.Length ~ Petal.Width,  data = iris),
#'              plot(Petal.Length ~ Sepal.Length, data = iris),
#'              plot(Petal.Width  ~ Sepal.Width,  data = iris),
#'              nrow = 2, ncol = 2)
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#' 
#' @md
plot_arrange <- function(..., nrow, ncol, par_args = character(0)) {
    initial_par <- graphics::par()
    shush(do.call(graphics::par, args = append(list(mfrow = c(nrow, ncol)), par_args)))
    sapply(list(...), eval)
    shush(do.call(graphics::par, args = initial_par))
}



# Colour palettes -------------------------------------------------------------------

#' Build a palette of colours from a list of hex codes
#'
#' `build_palette()` is a function that takes a vector of hex colours and lets you
#' randomise and return subsets of those colours. All of the other `palette_...()`
#' functions documented in the _Functions_ section below use pre-compiled lists
#' of hex colours (i.e. the `col_list` argument is already provided). All functions work
#' identically and return the same kind of data.
#'
#' @param col_list (Character) A vector of colours in RGB Hex format without transparency.
#' @param n (Numeric or `NULL`) The number of colours to return. If `NULL`, return all of
#'    the colours in `col_list`. If `n` is greater than the length of the colour list,
#'    triggers a warning and returns all of the colours.
#' @param random (Logical) If `TRUE`, colours will be randomly selected. If `FALSE`, they
#'    will be drawn from `col_list` in order.
#' @param alpha (Numeric) A single decimal value that modifies the opacity of the colours.
#'    `alpha = 0.65` makes all of the colours 65 percent opaque (35 percent transparent).
#'    If `alpha = NULL`, no alpha channel is added and the colours are 100 percent opaque.
#' @param spaced (Logical) If `TRUE`, the `n` chosen colours will be distributed throughout
#'    the `col_list`, e.g. 3 colours from a list of 16 will return 1, 8, and 16th colours.
#'    If `FALSE` (by default), colours will not be spaced out.
#'
#' @return A character vector of hex colours. If `alpha = NULL`, the colours will be in
#'    RGB Hex format (e.g. #FFFF00). If `alpha` is not `NULL`, the colours will be in
#'    RGBA Hex format (e.g. #FFFF00CB). If the `col_list` was a named vector, the names
#'    will be preserved.
#' @export
#'
#' @section Authors:
#' - Desi Quintans (<http://www.desiquintans.com>)
#'
#' @examples
#' # To see all of the colours (ordered left-to-right and top-to-bottom):
#' show_colours(palette_builtin())
#'
#' # To get the first 4 colours:
#' palette_builtin(4)
#'
#' #>     white     aliceblue  antiquewhite    antiquewhite1
#' #> "#FFFFFF"     "#F0F8FF"     "#FAEBD7"        "#FFEFDB"
#'
#' # To pick 4 colours randomly:
#' palette_builtin(4, random = TRUE)
#'
#' #>    gray52       coral4    darkorchid2      orchid4
#' #> "#858585"    "#8B3E2F"      "#B23AEE"    "#8B4789"
#'
#' # To pick 4 colours distributed evenly throughout the colour list:
#' palette_builtin(4, spaced = TRUE)
#'
#' #>     white        gray32    mediumpurple3       yellow4
#' #> "#FFFFFF"     "#525252"        "#8968CD"     "#8B8B00"
#'
#' # To make the colours 75 percent opaque (note that all args can work together):
#' palette_builtin(4, random = TRUE, spaced = TRUE, alpha = 0.75)
#'
#' #>      gray35          gray7          plum2     peachpuff3
#' #> "#595959BF"    "#121212BF"    "#EEAEEEBF"    "#CDAF95BF"
#'
#' # ------------
#'
#' # To use your own colour list, use build_palette():
#' build_palette(c("#000000", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46"), alpha = 0.5)
#'
#' #> [1] "#00000080" "#FFFF0080" "#1CE6FF80" "#FF34FF80" "#FF4A4680"
#'
#' @md
build_palette <- function(col_list, n = NULL, random = FALSE, spaced = FALSE, alpha = NULL) {
    # Default to returning all of the colours in the list if no specific number is chosen,
    # or if too many colours are requested.
    list_length <- length(col_list)

    if (is.null(n) == TRUE) {
        n <- list_length
    } else if (n <= 0 | is.numeric(n) == FALSE) {
        stop("You need to request at least 1 colour, or leave the 'n' argument as NULL\n",
             "  to request all colours by default.")
    } else if (list_length < n) {
        warning(n, " colours were requested, but there are only ", list_length, " colours ",
                "in the list. \n",
                "  All ", list_length, " colours will be returned.")
        n <- list_length
    }

    # 1. Build transparency info
    if (is.null(alpha) == TRUE) {
        alpha_data <- ""
    } else {
        # The decimal is converted to hex and then appended to the end of the colour code.
        alpha_data <- format(as.hexmode(round(255 * alpha[1])), upper.case = TRUE)
    }

    # 2. Randomise colours if needed.
    if (random == TRUE) {
        cols <- sample(col_list)
    } else {
        cols <- col_list
    }

    # 2. Then, take every nth element from the list.
    if (spaced == TRUE) {
        nth <- floor(seq(1, list_length, length.out = n))  # n elements, spaced evenly.

        # Note that n is never greater than list_length because of the length check at the
        # start of the function, so this function will return every 1th element if too
        # many n are requested.
    } else {
        nth <- seq(1, n)  # Return every 1th element up to n elements.
    }

    # 3. Select the colours
    cols <- cols[nth]

    # paste() destroys any names that are attached to the list. Need to save them first
    # and reapply them later.
    col_names <- names(cols)
    cols <- paste0(cols, alpha_data)
    names(cols) <- col_names

    return(cols)
}

#' @describeIn build_palette R's 502 built-in distinct named colours in hex form (the
#'    same ones that would be output by `colours(distinct = TRUE)`. The biggest
#'    advantage of accessing R's colours with this function is that you can add
#'    transparency to them.
#'
#' @section Authors:
#' - R Core Team
#'
#' @export
#' @md
palette_builtin <- function(n = NULL, random = FALSE, spaced = FALSE, alpha = NULL) {
    build_palette(rcols_as_hex(distinct = TRUE),
                  n = n, random = random, spaced = spaced, alpha = alpha)
}



#' @describeIn build_palette Tatarize's 1,022 visually-distinct colours
#'    (<https://stackoverflow.com/a/12224359/5578429>). Many of these are not
#'    colorblind safe, and many of them have low contrast or are very similar (but should
#'    still be different enough to discriminate when they are side-by-side).
#'
#' @section Authors:
#' - Tatarize (<https://stackoverflow.com/users/631911/tatarize>)
#'
#' @export
#' @md
palette_distinct <- function(n = NULL, random = FALSE, spaced = FALSE, alpha = NULL) {
    distinct_colours <- c(
        "#000000", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
        "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
        "#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
        "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100",
        "#DDEFFF", "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F",
        "#372101", "#FFB500", "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
        "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
        "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C",
        "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
        "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00",
        "#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700",
        "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
        "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C",
        "#83AB58", "#001C1E", "#D1F7CE", "#004B28", "#C8D0F6", "#A3A489", "#806C66", "#222800",
        "#BF5650", "#E83000", "#66796D", "#DA007C", "#FF1A59", "#8ADBB4", "#1E0200", "#5B4E51",
        "#C895C5", "#320033", "#FF6832", "#66E1D3", "#CFCDAC", "#D0AC94", "#7ED379", "#012C58",
        "#7A7BFF", "#D68E01", "#353339", "#78AFA1", "#FEB2C6", "#75797C", "#837393", "#943A4D",
        "#B5F4FF", "#D2DCD5", "#9556BD", "#6A714A", "#001325", "#02525F", "#0AA3F7", "#E98176",
        "#DBD5DD", "#5EBCD1", "#3D4F44", "#7E6405", "#02684E", "#962B75", "#8D8546", "#9695C5",
        "#E773CE", "#D86A78", "#3E89BE", "#CA834E", "#518A87", "#5B113C", "#55813B", "#E704C4",
        "#00005F", "#A97399", "#4B8160", "#59738A", "#FF5DA7", "#F7C9BF", "#643127", "#513A01",
        "#6B94AA", "#51A058", "#A45B02", "#1D1702", "#E20027", "#E7AB63", "#4C6001", "#9C6966",
        "#64547B", "#97979E", "#006A66", "#391406", "#F4D749", "#0045D2", "#006C31", "#DDB6D0",
        "#7C6571", "#9FB2A4", "#00D891", "#15A08A", "#BC65E9", "#FFFFFE", "#C6DC99", "#203B3C",
        "#671190", "#6B3A64", "#F5E1FF", "#FFA0F2", "#CCAA35", "#374527", "#8BB400", "#797868",
        "#C6005A", "#3B000A", "#C86240", "#29607C", "#402334", "#7D5A44", "#CCB87C", "#B88183",
        "#AA5199", "#B5D6C3", "#A38469", "#9F94F0", "#A74571", "#B894A6", "#71BB8C", "#00B433",
        "#789EC9", "#6D80BA", "#953F00", "#5EFF03", "#E4FFFC", "#1BE177", "#BCB1E5", "#76912F",
        "#003109", "#0060CD", "#D20096", "#895563", "#29201D", "#5B3213", "#A76F42", "#89412E",
        "#1A3A2A", "#494B5A", "#A88C85", "#F4ABAA", "#A3F3AB", "#00C6C8", "#EA8B66", "#958A9F",
        "#BDC9D2", "#9FA064", "#BE4700", "#658188", "#83A485", "#453C23", "#47675D", "#3A3F00",
        "#061203", "#DFFB71", "#868E7E", "#98D058", "#6C8F7D", "#D7BFC2", "#3C3E6E", "#D83D66",
        "#2F5D9B", "#6C5E46", "#D25B88", "#5B656C", "#00B57F", "#545C46", "#866097", "#365D25",
        "#252F99", "#00CCFF", "#674E60", "#FC009C", "#92896B", "#1E2324", "#DEC9B2", "#9D4948",
        "#85ABB4", "#342142", "#D09685", "#A4ACAC", "#00FFFF", "#AE9C86", "#742A33", "#0E72C5",
        "#AFD8EC", "#C064B9", "#91028C", "#FEEDBF", "#FFB789", "#9CB8E4", "#AFFFD1", "#2A364C",
        "#4F4A43", "#647095", "#34BBFF", "#807781", "#920003", "#B3A5A7", "#018615", "#F1FFC8",
        "#976F5C", "#FF3BC1", "#FF5F6B", "#077D84", "#F56D93", "#5771DA", "#4E1E2A", "#830055",
        "#02D346", "#BE452D", "#00905E", "#BE0028", "#6E96E3", "#007699", "#FEC96D", "#9C6A7D",
        "#3FA1B8", "#893DE3", "#79B4D6", "#7FD4D9", "#6751BB", "#B28D2D", "#E27A05", "#DD9CB8",
        "#AABC7A", "#980034", "#561A02", "#8F7F00", "#635000", "#CD7DAE", "#8A5E2D", "#FFB3E1",
        "#6B6466", "#C6D300", "#0100E2", "#88EC69", "#8FCCBE", "#21001C", "#511F4D", "#E3F6E3",
        "#FF8EB1", "#6B4F29", "#A37F46", "#6A5950", "#1F2A1A", "#04784D", "#101835", "#E6E0D0",
        "#FF74FE", "#00A45F", "#8F5DF8", "#4B0059", "#412F23", "#D8939E", "#DB9D72", "#604143",
        "#B5BACE", "#989EB7", "#D2C4DB", "#A587AF", "#77D796", "#7F8C94", "#FF9B03", "#555196",
        "#31DDAE", "#74B671", "#802647", "#2A373F", "#014A68", "#696628", "#4C7B6D", "#002C27",
        "#7A4522", "#3B5859", "#E5D381", "#FFF3FF", "#679FA0", "#261300", "#2C5742", "#9131AF",
        "#AF5D88", "#C7706A", "#61AB1F", "#8CF2D4", "#C5D9B8", "#9FFFFB", "#BF45CC", "#493941",
        "#863B60", "#B90076", "#003177", "#C582D2", "#C1B394", "#602B70", "#887868", "#BABFB0",
        "#030012", "#D1ACFE", "#7FDEFE", "#4B5C71", "#A3A097", "#E66D53", "#637B5D", "#92BEA5",
        "#00F8B3", "#BEDDFF", "#3DB5A7", "#DD3248", "#B6E4DE", "#427745", "#598C5A", "#B94C59",
        "#8181D5", "#94888B", "#FED6BD", "#536D31", "#6EFF92", "#E4E8FF", "#20E200", "#FFD0F2",
        "#4C83A1", "#BD7322", "#915C4E", "#8C4787", "#025117", "#A2AA45", "#2D1B21", "#A9DDB0",
        "#FF4F78", "#528500", "#009A2E", "#17FCE4", "#71555A", "#525D82", "#00195A", "#967874",
        "#555558", "#0B212C", "#1E202B", "#EFBFC4", "#6F9755", "#6F7586", "#501D1D", "#372D00",
        "#741D16", "#5EB393", "#B5B400", "#DD4A38", "#363DFF", "#AD6552", "#6635AF", "#836BBA",
        "#98AA7F", "#464836", "#322C3E", "#7CB9BA", "#5B6965", "#707D3D", "#7A001D", "#6E4636",
        "#443A38", "#AE81FF", "#489079", "#897334", "#009087", "#DA713C", "#361618", "#FF6F01",
        "#006679", "#370E77", "#4B3A83", "#C9E2E6", "#C44170", "#FF4526", "#73BE54", "#C4DF72",
        "#ADFF60", "#00447D", "#DCCEC9", "#BD9479", "#656E5B", "#EC5200", "#FF6EC2", "#7A617E",
        "#DDAEA2", "#77837F", "#A53327", "#608EFF", "#B599D7", "#A50149", "#4E0025", "#C9B1A9",
        "#03919A", "#1B2A25", "#E500F1", "#982E0B", "#B67180", "#E05859", "#006039", "#578F9B",
        "#305230", "#CE934C", "#B3C2BE", "#C0BAC0", "#B506D3", "#170C10", "#4C534F", "#224451",
        "#3E4141", "#78726D", "#B6602B", "#200441", "#DDB588", "#497200", "#C5AAB6", "#033C61",
        "#71B2F5", "#A9E088", "#4979B0", "#A2C3DF", "#784149", "#2D2B17", "#3E0E2F", "#57344C",
        "#0091BE", "#E451D1", "#4B4B6A", "#5C011A", "#7C8060", "#FF9491", "#4C325D", "#005C8B",
        "#E5FDA4", "#68D1B6", "#032641", "#140023", "#8683A9", "#CFFF00", "#A72C3E", "#34475A",
        "#B1BB9A", "#B4A04F", "#8D918E", "#A168A6", "#813D3A", "#425218", "#DA8386", "#776133",
        "#563930", "#8498AE", "#90C1D3", "#B5666B", "#9B585E", "#856465", "#AD7C90", "#E2BC00",
        "#E3AAE0", "#B2C2FE", "#FD0039", "#009B75", "#FFF46D", "#E87EAC", "#DFE3E6", "#848590",
        "#AA9297", "#83A193", "#577977", "#3E7158", "#C64289", "#EA0072", "#C4A8CB", "#55C899",
        "#E78FCF", "#004547", "#F6E2E3", "#966716", "#378FDB", "#435E6A", "#DA0004", "#1B000F",
        "#5B9C8F", "#6E2B52", "#011115", "#E3E8C4", "#AE3B85", "#EA1CA9", "#FF9E6B", "#457D8B",
        "#92678B", "#00CDBB", "#9CCC04", "#002E38", "#96C57F", "#CFF6B4", "#492818", "#766E52",
        "#20370E", "#E3D19F", "#2E3C30", "#B2EACE", "#F3BDA4", "#A24E3D", "#976FD9", "#8C9FA8",
        "#7C2B73", "#4E5F37", "#5D5462", "#90956F", "#6AA776", "#DBCBF6", "#DA71FF", "#987C95",
        "#52323C", "#BB3C42", "#584D39", "#4FC15F", "#A2B9C1", "#79DB21", "#1D5958", "#BD744E",
        "#160B00", "#20221A", "#6B8295", "#00E0E4", "#102401", "#1B782A", "#DAA9B5", "#B0415D",
        "#859253", "#97A094", "#06E3C4", "#47688C", "#7C6755", "#075C00", "#7560D5", "#7D9F00",
        "#C36D96", "#4D913E", "#5F4276", "#FCE4C8", "#303052", "#4F381B", "#E5A532", "#706690",
        "#AA9A92", "#237363", "#73013E", "#FF9079", "#A79A74", "#029BDB", "#FF0169", "#C7D2E7",
        "#CA8869", "#80FFCD", "#BB1F69", "#90B0AB", "#7D74A9", "#FCC7DB", "#99375B", "#00AB4D",
        "#ABAED1", "#BE9D91", "#E6E5A7", "#332C22", "#DD587B", "#F5FFF7", "#5D3033", "#6D3800",
        "#FF0020", "#B57BB3", "#D7FFE6", "#C535A9", "#260009", "#6A8781", "#A8ABB4", "#D45262",
        "#794B61", "#4621B2", "#8DA4DB", "#C7C890", "#6FE9AD", "#A243A7", "#B2B081", "#181B00",
        "#286154", "#4CA43B", "#6A9573", "#A8441D", "#5C727B", "#738671", "#D0CFCB", "#897B77",
        "#1F3F22", "#4145A7", "#DA9894", "#A1757A", "#63243C", "#ADAAFF", "#00CDE2", "#DDBC62",
        "#698EB1", "#208462", "#00B7E0", "#614A44", "#9BBB57", "#7A5C54", "#857A50", "#766B7E",
        "#014833", "#FF8347", "#7A8EBA", "#274740", "#946444", "#EBD8E6", "#646241", "#373917",
        "#6AD450", "#81817B", "#D499E3", "#979440", "#011A12", "#526554", "#B5885C", "#A499A5",
        "#03AD89", "#B3008B", "#E3C4B5", "#96531F", "#867175", "#74569E", "#617D9F", "#E70452",
        "#067EAF", "#A697B6", "#B787A8", "#9CFF93", "#311D19", "#3A9459", "#6E746E", "#B0C5AE",
        "#84EDF7", "#ED3488", "#754C78", "#384644", "#C7847B", "#00B6C5", "#7FA670", "#C1AF9E",
        "#2A7FFF", "#72A58C", "#FFC07F", "#9DEBDD", "#D97C8E", "#7E7C93", "#62E674", "#B5639E",
        "#FFA861", "#C2A580", "#8D9C83", "#B70546", "#372B2E", "#0098FF", "#985975", "#20204C",
        "#FF6C60", "#445083", "#8502AA", "#72361F", "#9676A3", "#484449", "#CED6C2", "#3B164A",
        "#CCA763", "#2C7F77", "#02227B", "#A37E6F", "#CDE6DC", "#CDFFFB", "#BE811A", "#F77183",
        "#EDE6E2", "#CDC6B4", "#FFE09E", "#3A7271", "#FF7B59", "#4E4E01", "#4AC684", "#8BC891",
        "#BC8A96", "#CF6353", "#DCDE5C", "#5EAADD", "#F6A0AD", "#E269AA", "#A3DAE4", "#436E83",
        "#002E17", "#ECFBFF", "#A1C2B6", "#50003F", "#71695B", "#67C4BB", "#536EFF", "#5D5A48",
        "#890039", "#969381", "#371521", "#5E4665", "#AA62C3", "#8D6F81", "#2C6135", "#410601",
        "#564620", "#E69034", "#6DA6BD", "#E58E56", "#E3A68B", "#48B176", "#D27D67", "#B5B268",
        "#7F8427", "#FF84E6", "#435740", "#EAE408", "#F4F5FF", "#325800", "#4B6BA5", "#ADCEFF",
        "#9B8ACC", "#885138", "#5875C1", "#7E7311", "#FEA5CA", "#9F8B5B", "#A55B54", "#89006A",
        "#AF756F", "#2A2000", "#7499A1", "#FFB550", "#00011E", "#D1511C", "#688151", "#BC908A",
        "#78C8EB", "#8502FF", "#483D30", "#C42221", "#5EA7FF", "#785715", "#0CEA91", "#FFFAED",
        "#B3AF9D", "#3E3D52", "#5A9BC2", "#9C2F90", "#8D5700", "#ADD79C", "#00768B", "#337D00",
        "#C59700", "#3156DC", "#944575", "#ECFFDC", "#D24CB2", "#97703C", "#4C257F", "#9E0366",
        "#88FFEC", "#B56481", "#396D2B", "#56735F", "#988376", "#9BB195", "#A9795C", "#E4C5D3",
        "#9F4F67", "#1E2B39", "#664327", "#AFCE78", "#322EDF", "#86B487", "#C23000", "#ABE86B",
        "#96656D", "#250E35", "#A60019", "#0080CF", "#CAEFFF", "#323F61", "#A449DC", "#6A9D3B",
        "#FF5AE4", "#636A01", "#D16CDA", "#736060", "#FFBAAD", "#D369B4", "#FFDED6", "#6C6D74",
        "#927D5E", "#845D70", "#5B62C1", "#2F4A36", "#E45F35", "#FF3B53", "#AC84DD", "#762988",
        "#70EC98", "#408543", "#2C3533", "#2E182D", "#323925", "#19181B", "#2F2E2C", "#023C32",
        "#9B9EE2", "#58AFAD", "#5C424D", "#7AC5A6", "#685D75", "#B9BCBD", "#834357", "#1A7B42",
        "#2E57AA", "#E55199", "#316E47", "#CD00C5", "#6A004D", "#7FBBEC", "#F35691", "#D7C54A",
        "#62ACB7", "#CBA1BC", "#A28A9A", "#6C3F3B", "#FFE47D", "#DCBAE3", "#5F816D", "#3A404A",
        "#7DBF32", "#E6ECDC", "#852C19", "#285366", "#B8CB9C", "#0E0D00", "#4B5D56", "#6B543F",
        "#E27172", "#0568EC", "#2EB500", "#D21656", "#EFAFFF", "#682021", "#2D2011", "#DA4CFF",
        "#70968E", "#FF7B7D", "#4A1930", "#E8C282", "#E7DBBC", "#A68486", "#1F263C", "#36574E",
        "#52CE79", "#ADAAA9", "#8A9F45", "#6542D2", "#00FB8C", "#5D697B", "#CCD27F", "#94A5A1",
        "#790229", "#E383E6", "#7EA4C1", "#4E4452", "#4B2C00", "#620B70", "#314C1E", "#874AA6",
        "#E30091", "#66460A", "#EB9A8B", "#EAC3A3", "#98EAB3", "#AB9180", "#B8552F", "#1A2B2F",
        "#94DDC5", "#9D8C76", "#9C8333", "#94A9C9", "#392935", "#8C675E", "#CCE93A", "#917100",
        "#01400B", "#449896", "#1CA370", "#E08DA7", "#8B4A4E", "#667776", "#4692AD", "#67BDA8",
        "#69255C", "#D3BFFF", "#4A5132", "#7E9285", "#77733C", "#E7A0CC", "#51A288", "#2C656A",
        "#4D5C5E", "#C9403A", "#DDD7F3", "#005844", "#B4A200", "#488F69", "#858182", "#D4E9B9",
        "#3D7397", "#CAE8CE", "#D60034", "#AA6746", "#9E5585", "#BA6200"
    )

    build_palette(distinct_colours, n = n, random = random, spaced = spaced, alpha = alpha)
}



#' @describeIn build_palette Adam Morse's 16 web-safe colours (<https://clrs.cc>). Nicer
#'    replacements for the standard browser colours, with good contrast and readability
#'    even when overplotted.
#'
#' @section Authors:
#' - Adam Morse (<http://mrmrs.cc/>)
#'
#' @export
#' @md
palette_mrmrs <- function(n = NULL, random = FALSE, spaced = FALSE, alpha = NULL) {
    mrmrs_colours <- c(
        "navy" = "#001F3F", "blue" = "#0074D9", "aqua" = "#7FDBFF", "teal" = "#39CCCC",
        "olive" = "#3D9970", "green" = "#2ECC40", "lime" = "#01FF70",
        "yellow" = "#FFDC00", "orange" = "#FF851B", "red" = "#FF4136",
        "maroon" = "#85144B", "fuchsia" = "#F012BE", "purple" = "#B10DC9",
        "black" = "#111111", "gray" = "#AAAAAA", "silver" = "#DDDDDD"
    )

    build_palette(mrmrs_colours, n = n, random = random, spaced = spaced, alpha = alpha)
}



#' @describeIn build_palette Desi's 14 hand-picked colours from the `palette_distinct()`
#'    range. The palette created by `desiderata::palette_distinct()` has a lot of colours
#'    that are either so dark or so light that it's difficult to differentiate them next
#'    to each other. In addition, many of the colours are affected by adjacency effects
#'    where putting an intermediate colour between them makes them look the same. I went
#'    through the preview plots manually, randomising the order of colours and deleting
#'    colours that were visually similar until I ended up with a list of colours that were
#'    easy to differentiate.
#'
#' @export
#' @md
palette_picked <- function(n = NULL, random = FALSE, spaced = FALSE, alpha = NULL) {
    picked_colours <- c(
        "darkslateblue" = "#004754",
        "darkseagreen" = "#00AE7E",
        "darkorchid" = "#7E2DD2",
        "yellow" = "#FFE502",
        "magenta" = "#FF029D",
        "plum" = "#FFA6FE",
        "goldenrod" = "#FEC96D",
        "orangered" = "#BE0028",
        "darkorange" = "#FF6832",
        "skyblue" = "#8CD0FF",
        "palegreen" = "#C2FF99",
        "dodgerblue" = "#3B5DFF",
        "chartreuse" = "#4FC601",
        "grey10" = "#121212"
    )

    build_palette(picked_colours, n = n, random = random, spaced = spaced, alpha = alpha)
}



#' @describeIn build_palette 48 distant colours from all palettes. This palette
#'   is made by converting the hex colours from the other palettes to RGB and
#'   HSV, rounding those values to the same range (e.g. rounding RGB values to
#'   the nearest 50), and then keeping the unique values only. This is the same
#'   way that `colours(distinct = TRUE)` defines its distinct colours.
#'
#' @export
#' @md
palette_distant <- function(n = NULL, random = FALSE, spaced = FALSE, alpha = NULL) {
    cols <- c("#A24E3D", "#D1ACFE", "#40E0D0", "#AABC7A", "#3A9459", "#78C8EB", 
              "#A37F46", "#8502AA", "#785715", "#874AA6", "#425218", "#E451D1", 
              "#836BBA", "#3CB44B", "#457D8B", "#7DBF32", "#077D84", "#322EDF", 
              "#C7847B", "#72A58C", "#62ACB7", "#DFFB71", "#B506D3", "#E704C4", 
              "#CCE93A", "#77D796", "#FFE4E1", "#9B9EE2", "#C23000", "#7C8060", 
              "#5875C1", "#014833", "#33A02C", "#A45B02", "#A4E804", "#E3AAE0", 
              "#04F757", "#671190", "#6542D2", "#FDBF6F", "#5CACEE", "#DDBC62", 
              "#17FCE4", "#CD853F", "#0568EC", "#469990", "#4B3A83", "#62E674")
    
    build_palette(cols, n = n, random = random, spaced = spaced, alpha = alpha)
}



# ggplot2 helper functions ----------------------------------------------------------

#' Rotate and align ggplot2 axis tick labels
#'
#' @param angle (Numeric) Rotation angle. `-90` rotates 90 degrees clockwise, `90` rotates
#'    90 degrees anti-clockwise.
#' @param align (Numeric) Horizontal alignment. `0` left-aligns, `1`
#'    right-aligns, and `0.5` centers.
#' @param valign (Numeric) Vertical alignment. `0` top-aligns, `1` bottom-aligns,
#'    and `0.5` centers.
#'
#' @return  A ggplot2 theme object.
#' @export
#'
#' @examples
#' # library(ggplot2)
#' # ggplot(mpg, aes(manufacturer, cty)) + geom_boxplot() +
#' #     rotate_x_text() +
#' #     rotate_y_text()
#'
#' #> A plot with X axis labels rotated 90 degrees clockwise and left-aligned to the
#' #> bottom edge of the plot area, and the Y axis labels rotated 90 degrees counter-
#' #> clockwise. All axis labels are centered on the tick marks.
#'
#' @md
#' @name rotate_axis_text
rotate_x_text <- function(angle = -90, align = 0, valign = 0.25) {
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle,
                                                       hjust = align,
                                                       vjust = valign))
}

#' @rdname rotate_axis_text
#' @export
rotate_y_text <- function(angle = 90, align = 0.5, valign = 0.5) {
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = angle,
                                                       hjust = align,
                                                       vjust = valign))
}


#' Horizontally align ggplot2 title and subtitle
#'
#' @param align Horizontal alignment. `0` left-aligns, `1` right-aligns, `0.5` centers.
#'
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#'
#' # library(ggplot2)
#' # ggplot(mpg, aes(manufacturer, cty)) + geom_boxplot() +
#' #     labs(title = "This is a title", subtitle = "This is a subtitle") +
#' #     align_titles(align = 0.5)
#'
#' #> A plot with the title and subtitle centered.
#'
#' @md
align_titles <- function(align = 0) {
    ggplot2::theme(
        plot.title      = ggplot2::element_text(hjust = align),
        plot.subtitle   = ggplot2::element_text(hjust = align)
    )
}


#' Rotate and align ggplot2 facet labels
#'
#' @param angle (Numeric) Rotation angle. `-90` rotates 90 degrees clockwise, `90` rotates
#'    90 degrees anti-clockwise.
#' @param align (Numeric) Horizontal alignment. `0` left-aligns, `1`
#'    right-aligns, and `0.5` centers.
#' @param valign (Numeric) Vertical alignment. `0` top-aligns, `1` bottom-aligns,
#'    and `0.5` centers.
#'
#' @return  A ggplot2 theme object.
#' @export
#'
#' @examples
#' # plot <- ggplot(mpg, aes(cty, hwy)) + geom_point() + facet_grid(year ~ fl)
#' # plot +
#' #     rotate_x_facet_text(angle = 45, align = 0.5) +
#' #     rotate_y_facet_text(angle = 0, valign = 0.5)
#'
#' @md
#' @name rotate_facet_text
rotate_x_facet_text <- function(angle = 45, align = 0, valign = 0.25) {
    ggplot2::theme(strip.text.x = ggplot2::element_text(angle = angle,
                                                        hjust = align,
                                                        vjust = valign))
}


#' @rdname rotate_facet_text
#' @export
rotate_y_facet_text <- function(angle = 45, align = 0, valign = 0.25) {
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = angle,
                                                        hjust = align,
                                                        vjust = valign))
}

#' Make a dendrogram
#'
#' @param df (Dataframe or Matrix) The data, arranged in wide format. Non-numeric data
#'    will be coerced into `NA`s.
#' @param clust_method (Character) the agglomeration method to be used. This should be
#'    (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single", "complete",
#'    "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid"
#'    (= UPGMC).
#' @param dist_method (Character) the distance measure to be used. This must be one of
#'    "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski". Any
#'    unambiguous substring can be given.
#' @param labels (Character) Text to use as the tip labels.
#' @param main (Character) Chart title.
#' @param sub (Character) Chart subtitle.
#' @param cex (Numeric) Size magnification. `0.8` is 80 percent of the base value.
#'
#' @return An object of class `hclust`.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(stringsAsFactors = FALSE,
#'                  site = c("a", "b", "c", "d"),
#'                  spp1 = c(0L, 1L, 1L, 0L),
#'                  spp2 = c(1L, 1L, 1L, 0L),
#'                  spp3 = c(0L, 0L, 1L, 0L))
#'
#' dendro(df[2:4], labels = df$site)
#' }
#'
#' @md
dendro <- function(df, clust_method = "ave", dist_method = "euclidean", labels = NULL,
                   main = NULL, sub = NULL, cex = 0.8) {
    hc <- stats::hclust(stats::dist(df, method = dist_method), clust_method)
    
    graphics::plot(hc,
                   main = main,
                   sub = sub,
                   labels = labels,
                   cex = cex)

    return(hc)
}

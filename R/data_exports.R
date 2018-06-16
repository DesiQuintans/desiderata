# lists of things
# Creates the data that is going to be included with the package. These lists tend to
# be large, so I don't necessarily want users to have to load them at runtime.

#' The names of 197 common colours
#'
#' This is a character vector of common colour names, lowercased and sorted by word length
#' so that common colours like red and blue come earlier in the list.
#'
#' @examples
#' # data(basic_color_names)
#'
#' # print(basic_color_names)
#'
#' #>   [1] "red"                    "tan"                    "aqua"
#' #>   [4] "blue"                   "cyan"                   "erin"
#' #>   [7] "gold"                   "gray"                   "grey"
#' #>  [10] "jade"                   "lime"                   "navy"
#' #>  [13] "pear"                   "peru"                   "pink"
#' #>  [16] "plum"                   "puce"                   "rose"
#' #>  [19] "ruby"                   "snow"                   "teal"
#' #>  [22] "amber"                  "azure"                  ...
#'
#' @section Author:
#' - Desi Quintans (@eco_desi)
#'
#' @section Source:
#' - <https://simple.wikipedia.org/wiki/Web_color>
#' - <https://simple.wikipedia.org/wiki/List_of_colors>
#'
#' @docType data
#' @name basic_colour_names
#'
#' @md
NULL

#' @name basic_color_names
#'
#' @examples
#' #> data(basic_colour_names)
#'
#' @rdname basic_colour_names
NULL

make_basic_color_names_list <- function() {
    basic_color_names <- c("red", "tan", "aqua", "blue", "cyan", "erin", "gold", "gray",
                           "grey", "jade", "lime", "navy", "pear", "peru", "pink", "plum",
                           "puce", "rose", "ruby", "snow", "teal", "amber", "azure",
                           "beige", "black", "blush", "brown", "coral", "green", "ivory",
                           "khaki", "lemon", "lilac", "linen", "mauve", "ocher", "ochre",
                           "olive", "peach", "taupe", "wheat", "white", "bisque",
                           "bronze", "cerise", "coffee", "copper", "indigo", "maroon",
                           "orange", "orchid", "purple", "salmon", "sienna", "silver",
                           "tomato", "violet", "yellow", "apricot", "carmine", "crimson",
                           "emerald", "fuchsia", "magenta", "sangria", "scarlet",
                           "thistle", "amaranth", "amethyst", "burgundy", "cerulean",
                           "cornsilk", "dark red", "dim gray", "dim grey", "honeydew",
                           "hot pink", "lavender", "moccasin", "old lace", "sapphire",
                           "seashell", "sky blue", "viridian", "baby blue", "byzantium",
                           "champagne", "chocolate", "dark blue", "dark cyan",
                           "dark gray", "dark grey", "deep pink", "gainsboro",
                           "goldenrod", "harlequin", "navy blue", "orangered",
                           "raspberry", "sea green", "turquoise", "alice blue",
                           "aquamarine", "blue-green", "burly wood", "cadet blue",
                           "chartreuse", "dark green", "dark khaki", "fire brick",
                           "indian red", "lawn green", "light blue", "light cyan",
                           "light gray", "light grey", "light pink", "lime green",
                           "mint cream", "misty rose", "olive drab", "orange-red",
                           "pale green", "peach puff", "periwinkle", "red-violet",
                           "rosy brown", "royal blue", "slate blue", "slate gray",
                           "slate grey", "spring bud", "steel blue", "blue violet",
                           "blue-violet", "cobalt blue", "dark orange", "dark orchid",
                           "dark salmon", "dark violet", "desert sand", "dodger blue",
                           "ghost white", "light coral", "light green", "medium blue",
                           "papaya whip", "powder blue", "sandy brown", "white smoke",
                           "dark magenta", "floral white", "forest green", "green yellow",
                           "jungle green", "light salmon", "light yellow", "magenta rose",
                           "navajo white", "persian blue", "saddle brown", "spring green",
                           "yellow green", "antique white", "deep sky blue",
                           "electric blue", "lemon chiffon", "medium orchid",
                           "medium purple", "midnight blue", "prussian blue",
                           "dark goldenrod", "dark sea green", "dark turquoise",
                           "lavender blush", "light sky blue", "pale goldenrod",
                           "pale turquoise", "blanched almond", "cornflower blue",
                           "dark slate blue", "dark slate gray", "dark slate grey",
                           "light sea green", "pale violet red", "chartreuse green",
                           "dark olive green", "light slate gray", "light slate grey",
                           "light steel blue", "medium sea green", "medium turquoise",
                           "medium aquamarine", "medium slate blue", "medium violet red",
                           "medium spring green", "light goldenrod yellow")

    basic_colour_names <- basic_color_names

    devtools::use_data(basic_color_names, basic_colour_names, overwrite = TRUE)
}


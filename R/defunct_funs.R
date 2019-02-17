# Defunct funs

#' Round a number to a fixed decimal place length
#'
#' Use `round(vec, digits = ...)` instead.
#'
#' @export
#'
#' @md
round_to_places <- function(num, places = 2) {
    .Defunct("round",
             msg = "round_to_places() is defunct.\nUse round(vec, digits = ...) instead.")
}



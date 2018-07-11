#' Retrieve simple or rich color names
#'
#' @param cols a character vector of hexadecimal color codes, or (pending) a
#'     matrix with red, green, blue values as columns.
#' @param table lookup table complexity, "plain" contains 228 unique color names whereas
#'     "rich" calls an API that serves 17,375 color names.
#' @details When the 'rich' table is specified, the function calls the Color Names API
#'     (https://github.com/meodai/color-names), which currently contains ~0.10\% of all
#'     >16.7 million RGB colors. The closest color name is returned. Users may submit
#'     candidate color names at the URL above.
#'
#' @return A character vector containing the names of colors in 'col'
#' @export
#'
#' @examples
#' get_names(cols = "#ff23ef", table = "plain")
#' get_names(cols = "#ff23ef", table = "rich")
#'
#' get_names(cols = c("#123456", "#ffffff"), table = "rich")
#' get_names(cols = rainbow(5), table = "plain")

get_names <- function(cols, table = c("plain", "rich"), maxColorValue = 255) {
  check_internet()

  if (!is.character(cols)) stop("'cols' must be a character vector.")
  if (is.matrix(cols) & any(cols > maxColorValue)) {
    stop("Color value exceeds the maximum value.\nDid you forget to increase the 'maxColorValue' parameter?")
  }
  tab <- match.arg(table, choices = c("plain", "rich"), several.ok = FALSE)

  if (tab == "plain") {
    eudist <- pracma::pdist2(colorspace::coords(colorspace::hex2RGB(cols)) * 255,
                             as.matrix(colortable[, c("r", "g", "b")]))
    return(colortable$name[apply(eudist, 1, which.min)])
  } else if (tab == "rich") {
    cols_nohash <- gsub("#", "", cols)
    res <- httr::GET(url = paste0(base_url, paste(cols_nohash, collapse = ",")))
    check_status(res)
    return(tolower(jsonlite::fromJSON(rawToChar(res$content))$colors$name))
  }
}

#' Check whether a color palette is accessible to colorblind folks
#'
#' @param cols a character vector of hexadecimal color codes
#' @param cb type of colorblindness to simulate
#' @param severity intensity of colorblindness [0-1]
#'
#' @return an object of class cbAccess
#' @export
#'
#' @examples
#' check_accessible(cols = c("#ffffff", "#394cda"), cb = "all", severity = 1)

check_accessible <- function(cols, cb = c("all", "deutan", "protan", "tritan"),
                             severity = 1) {
  if (!all(is.hexColor(cols))) stop("cols must be a character vector containing hexadecimal color values")
  if (severity > 1) warning("severity > 1: setting to 1")
  if (severity < 0) warning("severity < 0: setting to 0")
  cb <- match.arg(cb)

}

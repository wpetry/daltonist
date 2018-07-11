#' @importFrom attempt stop_if_not
#' @importFrom curl has_internet
#' @importFrom colorscience deltaE2000
check_internet <- function() {
  stop_if_not(.x = has_internet(), msg = "Please check your internet connection")
}

#' @importFrom httr status_code
check_status <- function(res) {
  stop_if_not(.x = status_code(res),
              .p = ~ .x == 200,
              msg = "The API returned an error")
}

base_url <- "https://api.color.pizza/v1/"


#' Calculate pairwise delta E2000 distances for a color palette
#'
#' @param pal a character vector containing hexadecimal color values
#'
#' @return a numeric matrix of pairwise delta E2000 color distances
#' @export
#'
#' @examples
#' pal_dE00(c("#ffffff", "#000000", "#123456"))

pal_dE00 <- function(pal) {
  if(!all(is.hexColor(pal))) stop("pal must contain only hexadecimal color value (e.g., '#ffffff' for white")

  pairs_hex <- combn(pal, 2, simplify = FALSE)
  pairs_Lab <- lapply(pairs_hex, hex2Lab)

  mat <- diag(length(pal))
  rownames(mat) <- colnames(mat) <- pal

  mat[upper.tri(mat)] <- mat[lower.tri(mat)] <- unname(sapply(pairs_Lab,
                                                              function(x) deltaE2000(Lab1 = x[1, ],
                                                                                     Lab2 = x[2, ])))
  return(mat)
}

#' Convert hexadecimal color strings to CIE Lab coordinates
#'
#' @param x a character vector of hexadecimal color strings.
#' @param toRGB parameter options for intermediate conversion to sRGB. See
#'     ?colorspace::hex2RGB for details.
#' @param toXYZ parameter options for intermediate conversion to XYZ. See
#'     ?colorscience::RGB2XYZ for details.
#' @param toLab parameter options for intermediate conversion to CIE Lab. See
#'     ?colorscience::XYZ2Lab for details.
#'
#' @return a matrix of CIE Lab coordinates
#' @export
#'
#' @examples
#' hex2Lab(x = c("#ffffff", "#59ceff"))

hex2Lab <- function(x, toRGB = list(gamma = FALSE),
                    toXYZ = list(illuminant = "D65", observer = 2,
                                 RefWhite = get("XYZperfectreflectingdiffuser",
                                                envir = environment()),
                                 RGBModel = "sRGB",
                                 RefWhiteRGB = get("whitepointsRGB", envir =environment()),
                                 gamma = NA,
                                 RefWhiteIllum = get("XYZperfectreflectingdiffuser",
                                                     envir = environment()),
                                 CAT = "Bradford",
                                 CATarray = get("ChromaticAdaptation",
                                                envir = environment())),
                    toLab = list(illuminant = "D65", observer = 2,
                                 RefWhite = get("XYZperfectreflectingdiffuser",
                                                envir = environment()))) {
  if(!is.character(x)) stop("x must be a hexidecimal character vector.")

  do.call(XYZ2Lab,
          c(list(XYZmatrix = do.call(RGB2XYZ,
                                     c(list(RGBmatrix = coords(do.call(hex2RGB,
                                                                       c(list(x = x),
                                                                         toRGB)))),
                                       toXYZ))),
            toLab))
}

#' Test whether vector values are valid hexadecimal color value
#'
#' @param x a character vector
#'
#' @return a logical vector
#' @export
#'
#' @examples
#' is.hexadecimal(c("#ffffff", "#123456", "#123", "#zabcde"))
is.hexColor <- function(x) {
  if (class(x) != "character") stop("x must be a character vector")
  grepl("^#([0-9A-f]){6,8}$", x)
}

#' Remove alpha channel from hexadecimal color value
#'
#' @param x is a character vector of colors
#'
#' @return a character vector of colors with the alpha channel set to 'FF' (= 100%)
#' @export
#'
#' @examples
#' dealpha(rainbow(5, alpha = 0.2))

dealpha <- function(x) {
  if (!any(is.hexColor(x))) stop("x must be a character vector of hexadecimal strings")
  gsub(pattern = "([0-9A-f]){2}$", replacement = "FF", x)
}

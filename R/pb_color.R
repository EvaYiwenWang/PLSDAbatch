#' Color Palette for PLSDAbatch Plots
#'
#' This function returns a discrete color palette used in
#' \pkg{PLSDAbatch} plots. The palette combines hues from
#' \code{\link[mixOmics]{color.mixo}} and \code{\link[scales]{hue_pal}}
#' and contains at most 25 distinct colors.
#'
#' @importFrom mixOmics color.mixo
#' @importFrom scales hue_pal
#'
#' @param num.vector An integer vector of indices specifying which colors
#'   to extract from the internal palette. Valid values are between
#'   \code{1} and \code{25}.
#'
#' @return
#' A character vector of hexadecimal color codes.
#'
#' @author Yiwen Wang, Kim-Anh Le Cao
#'
#' @seealso
#' \code{\link{lighten}}, \code{\link{darken}},
#' \code{\link[mixOmics]{color.mixo}},
#' \code{\link[scales]{hue_pal}}
#'
#' @examples
#' pb_color(seq_len(5))
#' pb_color(c(1, 3, 5))
#'
#' @export
pb_color <- function(num.vector) {
    hex_codes1 <- scales::hue_pal(l = 65, c = 100)(10)
    hex_codes2 <- scales::hue_pal(l = 40, c = 60)(5)

    colorlist <- c(
        hex_codes1[c(1, 4, 7, 2, 8, 3, 9, 5, 10, 6)],
        mixOmics::color.mixo(c(1, 2, 6, 3, 9, 4, 5, 7, 10)),
        hex_codes2[seq_len(3)],
        mixOmics::color.mixo(8),
        hex_codes2[4:5]
    )

    if (any(num.vector < 1 | num.vector > length(colorlist))) {
        stop("num.vector must be between 1 and ", length(colorlist), ".")
    }

    colorlist[num.vector]
}


#' Lighten Colors by Increasing Brightness
#'
#' This function takes one or more colors and increases their brightness
#' by adding a constant amount to their RGB values. The adjustment is
#' performed in the RGB space, and the resulting values are truncated
#' to stay within \code{[0, 1]}. Alpha channels are not preserved and
#' the returned colors are fully opaque.
#'
#' @param colvec A vector of colors specified as character strings
#'   (for example, hexadecimal colors or any color name recognized by R).
#' @param amount A scalar numeric value indicating how much brightness
#'   to add. Must be between \code{0} and \code{1}. Default is \code{0.15}.
#'
#' @return A character vector of lightened colors in hexadecimal RGB format.
#'
#' @author Yiwen Wang, Kim-Anh Le Cao
#'
#' @seealso \code{\link{darken}}, \code{\link{pb_color}}
#'
#' @examples
#' lighten("#336699")
#' lighten(c("red", "blue"), amount = 0.1)
#' lighten(pb_color(seq_len(3)), amount = 0.2)
#'
#' @export
lighten <- function(colvec, amount = 0.15) {
    if (length(amount) != 1L ||
        !is.finite(amount) || amount < 0 || amount > 1) {
        stop("`amount` must be a finite scalar in [0, 1].")
    }

    rgb.mat <- t(grDevices::col2rgb(colvec) / 255)
    rgb.new <- pmin(rgb.mat + amount, 1)
    grDevices::rgb(rgb.new[, 1], rgb.new[, 2], rgb.new[, 3])
}


#' Darken Colors by Decreasing Brightness
#'
#' This function takes one or more colors and decreases their brightness
#' by subtracting a constant amount from their RGB values. The adjustment
#' is performed in the RGB space, and the resulting values are truncated
#' to stay within \code{[0, 1]}. Alpha channels are not preserved and
#' the returned colors are fully opaque.
#'
#' @param colvec A vector of colors specified as character strings
#'   (for example, hexadecimal colors or any color name recognized by R).
#' @param amount A scalar numeric value indicating how much brightness
#'   to subtract. Must be between \code{0} and \code{1}. Default is \code{0.15}.
#'
#' @return A character vector of darkened colors in hexadecimal RGB format.
#'
#' @author Yiwen Wang, Kim-Anh Le Cao
#'
#' @seealso \code{\link{lighten}}, \code{\link{pb_color}}
#'
#' @examples
#' darken("#336699")
#' darken(c("red", "blue"), amount = 0.1)
#' darken(pb_color(seq_len(3)), amount = 0.2)
#'
#' @export
darken <- function(colvec, amount = 0.15) {
    if (length(amount) != 1L ||
        !is.finite(amount) || amount < 0 || amount > 1) {
        stop("`amount` must be a finite scalar in [0, 1].")
    }

    rgb.mat <- t(grDevices::col2rgb(colvec) / 255)
    rgb.new <- pmax(rgb.mat - amount, 0)
    grDevices::rgb(rgb.new[, 1], rgb.new[, 2], rgb.new[, 3])
}

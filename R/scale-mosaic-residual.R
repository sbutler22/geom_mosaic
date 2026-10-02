#' Default diverging fill scale for mosaic residuals
#'
#' A continuous red-white-blue gradient (red = negative residual /
#' under-represented, blue = positive / over-represented), rather than base
#' R's discrete HSV-stepped bins. Chosen as the default because it reads
#' better alongside in-tile labels -- there's no hard color-bin boundary
#' cutting across a cell's text the way base R's stepped bins can. It's
#' still an ordinary ggplot2 scale object, so it composes and overrides the
#' usual way.
#'
#' @param low,mid,high Colors for the most negative, zero, and most positive
#'   residual.
#' @param midpoint Residual value treated as the color midpoint (`0` =
#'   independence).
#' @param name Legend title.
#' @param ... Passed on to [ggplot2::scale_fill_gradient2()].
#' @return A ggplot2 `ScaleContinuous` object.
#' @export
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   genre = sample(c("Action", "Sports", "Puzzle"), 200, replace = TRUE),
#'   platform = sample(c("PS", "Switch"), 200, replace = TRUE)
#' )
#' ggplot(df) +
#'   geom_mosaic_resid(aes(x = genre, y = platform), default_scale = FALSE) +
#'   scale_fill_mosaic_residual()
scale_fill_mosaic_residual <- function(low = "firebrick", mid = "white", high = "steelblue",
                                        midpoint = 0, name = "Residual", ...) {
  ggplot2::scale_fill_gradient2(low = low, mid = mid, high = high,
                                 midpoint = midpoint, name = name, ...)
}

#' StatMosaicResidual
#'
#' Takes discrete `catx`/`caty` aesthetics (and optional `weight`), computes
#' mosaic rectangles + independence residuals, and hands
#' `xmin`/`xmax`/`ymin`/`ymax` + `residual` off to a geom (`GeomRect` by
#' default).
#'
#' @format NULL
#' @usage NULL
#' @export
StatMosaicResidual <- ggplot2::ggproto("StatMosaicResidual", ggplot2::Stat,

  required_aes = c("catx", "caty"),

  # so users get residual-based fill for free, without writing
  # aes(fill = after_stat(residual)) themselves
  default_aes = ggplot2::aes(fill = ggplot2::after_stat(residual)),

  compute_panel = function(data, scales, spacing = 0.01, dir = NULL,
                            type = c("pearson", "deviance", "FT")) {

    type <- match.arg(type)
    if (!"weight" %in% names(data)) data$weight <- 1

    rects <- compute_mosaic_residuals(data, "catx", "caty", weight = "weight",
                                       spacing = spacing, dir = dir, type = type)

    rects$PANEL <- data$PANEL[1]
    rects$group <- seq_len(nrow(rects))  # each cell draws as its own rect

    rects
  }
)

#' Compute mosaic tiles and independence residuals as a ggplot2 stat
#'
#' The stat underlying [geom_mosaic_resid()]'s rectangles. Use this
#' directly if you want to pair the mosaic-residual computation with a geom
#' other than `GeomRect`.
#'
#' @inheritParams ggplot2::stat_identity
#' @param na.rm If `FALSE` (the default), missing values are removed with a
#'   warning; if `TRUE`, missing values are silently removed.
#' @param spacing Gap between sibling tiles, as a fraction of their parent's
#'   dimension.
#' @param dir Optional character vector of `"v"`/`"h"` (one per variable:
#'   here, `c(x_direction, y_direction)`) forcing the split direction at
#'   each level, matching base R's `mosaicplot(dir = )`. `NULL` (default)
#'   alternates v/h starting with v, same as base R's default.
#' @param type Residual formula: `"pearson"` (default), `"deviance"`, or
#'   `"FT"` (Freeman-Tukey), matching base R's `mosaicplot(type = )`.
#' @export
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   genre = sample(c("Action", "Sports", "Puzzle"), 200, replace = TRUE),
#'   platform = sample(c("PS", "Switch"), 200, replace = TRUE)
#' )
#' ggplot(df) + stat_mosaic_residual(aes(x = genre, y = platform))
stat_mosaic_residual <- function(mapping = NULL, data = NULL, geom = "rect",
                                  position = "identity", na.rm = FALSE,
                                  spacing = 0.01, dir = NULL,
                                  type = c("pearson", "deviance", "FT"),
                                  show.legend = NA, inherit.aes = TRUE, ...) {
  type <- match.arg(type)
  mapping <- remap_mosaic_aes(mapping)
  ggplot2::layer(
    stat = StatMosaicResidual,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, spacing = spacing, dir = dir, type = type, ...)
  )
}

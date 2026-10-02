#' StatMosaicLabel
#'
#' Computes label text + centroid position for each mosaic cell, so category
#' names can be drawn directly inside their rectangles. This sidesteps a
#' real constraint of mosaic plots: the inner variable's split positions
#' differ from column to column (they're conditional on the outer
#' variable), so there's no single shared axis tick that's correct for the
#' whole plot. In-tile labels are the structurally correct fix, not a
#' workaround.
#'
#' Cells smaller than `min_label_width`/`min_label_height` (in the same 0-1
#' fractional units as `xmin`/`xmax`/`ymin`/`ymax`) are dropped from the
#' label set entirely -- they're still colored by the rect layer, just with
#' no text crammed into a sliver. These are exposed as tunable parameters
#' because the "right" threshold depends on plot size, font size, and label
#' text length, not on the data alone.
#'
#' @format NULL
#' @usage NULL
#' @export
StatMosaicLabel <- ggplot2::ggproto("StatMosaicLabel", ggplot2::Stat,

  required_aes = c("catx", "caty"),

  compute_panel = function(data, scales, spacing = 0.01,
                            min_label_width = 0.02, min_label_height = 0.03,
                            dir = NULL) {

    if (!"weight" %in% names(data)) data$weight <- 1

    rects <- compute_mosaic_residuals(data, "catx", "caty", weight = "weight",
                                       spacing = spacing, dir = dir)

    rects$x <- (rects$xmin + rects$xmax) / 2
    rects$y <- (rects$ymin + rects$ymax) / 2
    rects$label <- paste(rects$catx, rects$caty, sep = "\n")

    # drop labels for cells too small to hold text legibly -- the color
    # still shows, there's just no text crammed into a sliver
    width  <- rects$xmax - rects$xmin
    height <- rects$ymax - rects$ymin
    rects <- rects[width >= min_label_width & height >= min_label_height, , drop = FALSE]

    rects$PANEL <- data$PANEL[1]
    rects$group <- seq_len(nrow(rects))

    rects
  }
)

#' Compute in-tile mosaic labels as a ggplot2 stat
#'
#' The stat underlying [geom_mosaic_resid()]'s in-tile category labels.
#' Use this directly if you want the labels without the default rectangles
#' and fill scale.
#'
#' @inheritParams ggplot2::stat_identity
#' @param na.rm If `FALSE` (the default), missing values are removed with a
#'   warning; if `TRUE`, missing values are silently removed.
#' @param spacing Gap between sibling tiles, as a fraction of their parent's
#'   dimension -- must match whatever `spacing` was passed to the paired
#'   `stat_mosaic_residual()`/`geom_mosaic_resid()` call, so labels land
#'   on the same rectangles.
#' @param min_label_width,min_label_height Minimum cell width/height (in 0-1
#'   fractional plot units) required for a label to be drawn at all. Tune
#'   these per-plot: smaller plots or longer category names typically need
#'   larger thresholds to avoid overflow.
#' @param dir Optional character vector of `"v"`/`"h"` forcing split
#'   direction per level -- must match whatever `dir` was passed to the
#'   paired `stat_mosaic_residual()`/`geom_mosaic_resid()` call, so
#'   labels land on the same rectangles. `NULL` (default) alternates v/h.
#' @export
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   genre = sample(c("Action", "Sports", "Puzzle"), 200, replace = TRUE),
#'   platform = sample(c("PS", "Switch"), 200, replace = TRUE)
#' )
#' ggplot(df) +
#'   stat_mosaic_residual(aes(x = genre, y = platform)) +
#'   stat_mosaic_label(aes(x = genre, y = platform))
stat_mosaic_label <- function(mapping = NULL, data = NULL, geom = "text",
                               position = "identity", na.rm = FALSE,
                               spacing = 0.01, min_label_width = 0.02,
                               min_label_height = 0.03, dir = NULL,
                               show.legend = FALSE, inherit.aes = TRUE, ...) {
  mapping <- remap_mosaic_aes(mapping)
  ggplot2::layer(
    stat = StatMosaicLabel,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, spacing = spacing,
                  min_label_width = min_label_width,
                  min_label_height = min_label_height, dir = dir, ...)
  )
}

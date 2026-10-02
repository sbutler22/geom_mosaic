#' mosaicresid: Residual-Shaded Mosaic Plots for ggplot2
#'
#' A ggplot2-native alternative to base R's `mosaicplot(shade = TRUE)`.
#' [geom_mosaic_resid()] takes two categorical variables mapped the usual
#' ggplot2 way, `aes(x =, y =)`, recursively partitions the unit square into
#' tiles sized by their joint proportions, computes independence-model
#' residuals, and maps them to fill color automatically.
#'
#' @section Main functions:
#' - [geom_mosaic_resid()] -- the all-in-one layer: tiles, in-tile labels,
#'   and a default diverging color scale.
#' - [stat_mosaic_residual()] / [stat_mosaic_label()] -- the underlying stats,
#'   if you want to pair them with a different geom.
#' - [compute_mosaic_residuals()] -- the plain-data-frame version, with no
#'   ggplot2 involved at all, for inspecting the tile geometry and residuals
#'   directly.
#' - [scale_fill_mosaic_residual()] -- the default diverging fill scale,
#'   usable standalone.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
## usethis namespace: end
NULL

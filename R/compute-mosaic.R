#' Compute mosaic tile geometry and independence residuals
#'
#' The plain-data-frame engine behind [geom_mosaic_resid()], with no
#' ggplot2 involved: converts `data` to long-format counts, recursively
#' partitions the unit square into tiles sized by joint proportion, and
#' attaches independence-model residuals to each tile. Returned as an
#' ordinary tibble, so it's directly inspectable -- useful for checking the
#' underlying numbers, or for driving a completely custom plot.
#'
#' @param data A data frame -- either one row per observation, or already
#'   aggregated with an existing count/weight column.
#' @param x,y Strings naming the two grouping variables.
#' @param weight Optional string naming an existing count column.
#' @param spacing Gap size (fraction of each split's dimension) between
#'   sibling rectangles at every level, passed through to the internal
#'   partitioning step. `0` = no gap.
#' @param dir Optional character vector of `"v"`/`"h"` (one per variable,
#'   i.e. length 2 here) forcing the split direction at each level, matching
#'   base R's `mosaicplot(dir = )`. `NULL` (default) alternates v/h starting
#'   with v, same as base R's default.
#' @param type Residual formula: `"pearson"` (default), `"deviance"`, or
#'   `"FT"` (Freeman-Tukey), matching base R's `mosaicplot(type = )`.
#' @return A tibble with one row per `(x, y)` cell: `xmin`/`xmax`/`ymin`/
#'   `ymax` (tile bounds in `[0,1]` plot units), `n` (cell count), `level`,
#'   the `x`/`y` category columns, `row_total`/`col_total`/`expected`, and
#'   `residual`.
#' @export
#' @examples
#' df <- data.frame(
#'   genre = sample(c("Action", "Sports", "Puzzle"), 200, replace = TRUE),
#'   platform = sample(c("PS", "Switch"), 200, replace = TRUE)
#' )
#' compute_mosaic_residuals(df, "genre", "platform")
compute_mosaic_residuals <- function(data, x, y, weight = NULL, spacing = 0.01,
                                      dir = NULL, type = c("pearson", "deviance", "FT")) {
  type <- match.arg(type)
  counts <- to_long_counts(data, x, y, weight)
  rects  <- partition_rects(counts, vars = c(x, y), weight = "n", spacing = spacing, dir = dir)
  add_independence_residuals(rects, x, y, type = type)
}

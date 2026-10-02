#' Move x/y out of the way of ggplot2's real position aesthetic
#'
#' Renames `mapping$x` to `mapping$catx` and `mapping$y` to `mapping$caty`
#' before a layer's mapping is inspected for scale-training purposes.
#'
#' Without this, ggplot2 sees the raw categorical `x`/`y` values at mapping
#' time and locks the position scales as discrete -- then errors when the
#' stat hands back continuous `xmin`/`xmax`/`ymin`/`ymax` for the mosaic
#' tiles. Internal to every `stat_mosaic_*()`/`geom_mosaic_residual()` call;
#' end users never call this directly.
#'
#' @param mapping an `aes()` mapping, as passed to `stat_mosaic_residual()`,
#'   `stat_mosaic_label()`, or `geom_mosaic_residual()`.
#' @return The mapping with `x`/`y` renamed to `catx`/`caty`.
#' @noRd
remap_mosaic_aes <- function(mapping) {
  if (!is.null(mapping$x)) {
    mapping$catx <- mapping$x
    mapping$x <- NULL
  }
  if (!is.null(mapping$y)) {
    mapping$caty <- mapping$y
    mapping$y <- NULL
  }
  mapping
}

#' Residual-shaded mosaic plot
#'
#' The main entry point for this package. A ggplot2-native equivalent of
#' base R's `mosaicplot(shade = TRUE)`: give it two categorical variables
#' the normal ggplot2 way, `aes(x =, y =)`, and it draws a true mosaic plot
#' (tiles sized by joint proportion, not a grid approximation), shaded by
#' independence-model residuals, with in-tile category labels -- no manual
#' stat work required.
#'
#' Returns a `list` of ggplot2 layers/scales -- the rectangles (`GeomRect`),
#' in-tile category labels (`GeomText`), and a default diverging fill scale
#' -- so a single call gives you all three. ggplot2 natively supports
#' `+`-ing a list of layers/scales onto a plot, so this composes just like a
#' normal single-layer geom would.
#'
#' @inheritParams ggplot2::geom_rect
#' @param spacing Gap between sibling tiles, as a fraction of their parent's
#'   dimension.
#' @param label Set to `FALSE` to get only the rectangles, no text labels.
#' @param min_label_width,min_label_height Minimum cell width/height (0-1
#'   fractional units) required for a label to be drawn; smaller cells are
#'   colored but left unlabeled rather than overflowing.
#' @param label.size,label.color Passed through to the label's
#'   `geom_text()` layer.
#' @param default_scale Set `FALSE` to skip the built-in color scale
#'   entirely (e.g. if you always add your own).
#' @param scale_low,scale_mid,scale_high,scale_midpoint Passed through to
#'   the default scale ([scale_fill_mosaic_residual()]).
#' @param dir Optional character vector of `"v"`/`"h"` (one per variable,
#'   i.e. `c(x_direction, y_direction)`) forcing the split direction at each
#'   level -- matches base R's `mosaicplot(dir = )`. `NULL` (default)
#'   alternates v/h starting with v, same as base R's default.
#' @param type Residual formula: `"pearson"` (default), `"deviance"`, or
#'   `"FT"` (Freeman-Tukey) -- matches base R's `mosaicplot(type = )`.
#' @return A `list` of ggplot2 layers (and, by default, a scale), suitable
#'   for adding to a `ggplot()` call with `+`.
#' @export
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   genre = sample(c("Action", "Sports", "Puzzle"), 200, replace = TRUE),
#'   platform = sample(c("PS", "Switch"), 200, replace = TRUE)
#' )
#'
#' # colors, spacing, and labels all come for free -- no manual
#' # scale_fill_gradient2() call needed, it's the default
#' ggplot(df) +
#'   geom_mosaic_resid(aes(x = genre, y = platform)) +
#'   theme_minimal()
#'
#' # the default scale is fully overridable like any ggplot2 layer
#' ggplot(df) +
#'   geom_mosaic_resid(aes(x = genre, y = platform)) +
#'   scale_fill_gradient2(low = "purple", mid = "white", high = "darkgreen") +
#'   theme_minimal()
geom_mosaic_resid <- function(mapping = NULL, data = NULL,
                                  stat = "mosaic_residual", position = "identity",
                                  na.rm = FALSE, show.legend = NA,
                                  inherit.aes = TRUE, spacing = 0.01, label = TRUE,
                                  min_label_width = 0.02, min_label_height = 0.03,
                                  label.size = 3, label.color = "black",
                                  default_scale = TRUE,
                                  scale_low = "firebrick", scale_mid = "white",
                                  scale_high = "steelblue", scale_midpoint = 0,
                                  dir = NULL, type = c("pearson", "deviance", "FT"), ...) {

  type <- match.arg(type)
  mapping <- remap_mosaic_aes(mapping)

  rect_layer <- ggplot2::layer(
    geom = ggplot2::GeomRect,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, spacing = spacing, dir = dir, type = type, ...)
  )

  out <- list(rect_layer)

  if (label) {
    text_layer <- ggplot2::layer(
      stat = StatMosaicLabel,
      data = data,
      mapping = mapping,
      geom = "text",
      position = "identity",
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, spacing = spacing,
                    min_label_width = min_label_width,
                    min_label_height = min_label_height, dir = dir,
                    size = label.size, colour = label.color)
    )
    out <- c(out, list(text_layer))
  }

  # Default fill scale -- still a normal ggplot2 scale object, so the user
  # can override it the usual way: geom_mosaic_resid(...) +
  # scale_fill_gradient2(...) (ggplot2 will just note the scale is being
  # replaced). Passing a literal `fill = ` through `...` (like any ggplot2
  # geom) hardcodes a constant color instead, same as usual.
  if (default_scale && !("fill" %in% names(list(...)))) {
    out <- c(out, list(scale_fill_mosaic_residual(
      low = scale_low, mid = scale_mid, high = scale_high, midpoint = scale_midpoint
    )))
  }

  # Always return a classed list, even when it holds a single layer --
  # the mosaicresid_layers class is what triggers the auto-weight-by-Freq
  # behavior in ggplot_add.mosaicresid_layers() when this is added with `+`
  # to a plot built from a table (via fortify.table()). Collapsing to a bare
  # Layer here would silently skip that behavior.
  structure(out, class = "mosaicresid_layers")
}

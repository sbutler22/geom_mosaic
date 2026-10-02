#' Add mosaicresid layers to a ggplot, auto-weighting by a `Freq` column
#'
#' [geom_mosaic_resid()] returns its rectangles/labels/scale as a classed
#' list so this method runs when it's added to a plot with `+`. Its only
#' job beyond normal list-of-layers addition: if the plot's data has a
#' column literally named `Freq` (the name `as.data.frame.table()` -- and
#' therefore [fortify.table()] -- always produces) and the user hasn't
#' already mapped `weight` themselves, treat `Freq` as the implicit cell
#' count. This is the closest `ggplot2`-idiomatic equivalent of base R's
#' `mosaicplot(x)`, where `x` is a table and its cell values already *are*
#' the counts -- no separate weighting step exists there because the table
#' format fuses category structure and counts into one object.
#'
#' End users never call this directly; it's invoked automatically by `+`.
#'
#' @param object A `mosaicresid_layers` list, as returned by
#'   [geom_mosaic_resid()].
#' @param plot The `ggplot` object being added to.
#' @param object_name The name of `object`, for error messages.
#' @return The updated `ggplot` object.
#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add mosaicresid_layers
#' @export
ggplot_add.mosaicresid_layers <- function(object, plot, object_name) {

  if (is.data.frame(plot$data) && "Freq" %in% names(plot$data)) {
    object <- lapply(object, function(component) {
      if (inherits(component, "LayerInstance") && is.null(component$mapping$weight)) {
        component$mapping$weight <- rlang::quo(.data$Freq)
      }
      component
    })
  }

  Reduce(function(p, component) ggplot2::ggplot_add(component, p, object_name), object, plot)
}

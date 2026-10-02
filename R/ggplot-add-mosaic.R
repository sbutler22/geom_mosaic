#' Add mosaicresid layers to a plot, auto-weighting by a `Freq` column
#'
#' Shared logic behind both extension points this package registers for
#' adding a `mosaicresid_layers` list (as returned by [geom_mosaic_resid()])
#' to a plot with `+` -- the legacy S3 `ggplot_add()` generic (`ggplot2` <
#' 4.0) and the newer S7 `update_ggplot()` generic (`ggplot2` >= 4.0, see
#' `zzz.R`). `ggplot2`'s internal rewrite changed *how* a package registers
#' this kind of extension, but not what it should do, so both entry points
#' call this one function.
#'
#' Its job: if the plot's data has a column literally named `Freq` (the
#' name `as.data.frame.table()` -- and therefore [fortify.table()] --
#' always produces) and the user hasn't already mapped `weight` themselves,
#' treat `Freq` as the implicit cell count. This is the closest
#' `ggplot2`-idiomatic equivalent of base R's `mosaicplot(x)`, where `x` is
#' a table and its cell values already *are* the counts -- no separate
#' weighting step exists there because the table format fuses category
#' structure and counts into one object.
#'
#' @param object A `mosaicresid_layers` list.
#' @param plot The `ggplot` object being added to.
#' @return The updated `ggplot` object.
#' @noRd
add_mosaicresid_layers <- function(object, plot) {

  if (is.data.frame(plot$data) && "Freq" %in% names(plot$data)) {
    object <- lapply(object, function(component) {
      if (inherits(component, "Layer") && is.null(component$mapping$weight)) {
        component$mapping$weight <- rlang::quo(.data$Freq)
      }
      component
    })
  }

  Reduce(function(p, component) p + component, object, plot)
}

#' Add mosaicresid layers to a plot (ggplot2 < 4.0 entry point)
#'
#' The classic S3 `ggplot_add()` method that handles `+`-ing a
#' `mosaicresid_layers` list onto a plot on `ggplot2` versions before the
#' 4.0 S7 rewrite. See `add_mosaicresid_layers()` for what this actually
#' does; on `ggplot2` >= 4.0 the equivalent work happens via the S7 method
#' registered in `zzz.R` instead, since this S3 method is never reached
#' there.
#'
#' @param object A `mosaicresid_layers` list, as returned by
#'   [geom_mosaic_resid()].
#' @param plot The `ggplot` object being added to.
#' @param object_name The name of `object`, for error messages (required by
#'   the `ggplot_add()` generic's signature; unused here).
#' @return The updated `ggplot` object.
#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add mosaicresid_layers
#' @export
ggplot_add.mosaicresid_layers <- function(object, plot, object_name) {
  add_mosaicresid_layers(object, plot)
}

#' Register S7-based plot-addition support on ggplot2 >= 4.0
#'
#' `ggplot2` 4.0 rewrote its internals around the `S7` object system. As
#' part of that, the classic S3 `ggplot_add()` generic became "vestigial":
#' its only remaining method, `ggplot_add.default`, just forwards to a new
#' S7 generic, `update_ggplot()`. An S3 method like
#' `ggplot_add.mosaicresid_layers()` (defined in `ggplot-add-mosaic.R`,
#' which still runs this package's own `+`-addition logic) is therefore
#' never reached on `ggplot2` >= 4.0 -- `UseMethod()` resolution happens
#' before an S3 method ever gets a chance to intercept the vestigial
#' default.
#'
#' To keep working across both `ggplot2` generations, this conditionally
#' registers a second entry point, an S7 method for `update_ggplot()`, only
#' when `ggplot2::update_ggplot()`/`ggplot2::class_ggplot` actually exist
#' (i.e. `ggplot2` >= 4.0 is installed) and the `S7` package is available
#' (which it always will be in that case, since `ggplot2` >= 4.0 itself
#' depends on `S7`). On `ggplot2` < 4.0, this check is simply skipped and
#' the classic S3 `ggplot_add.mosaicresid_layers()` method handles
#' everything, as it always did.
#'
#' `S7::new_S3_class()` is the documented bridge for letting an S7 generic
#' dispatch on an ordinary S3-classed object like the `mosaicresid_layers`
#' list [geom_mosaic_resid()] returns, so no part of this package's public
#' API needs to become an S7 object itself.
#'
#' @param libname,pkgname Standard `.onLoad()` arguments; supplied by R,
#'   not called directly.
#' @noRd
.onLoad <- function(libname, pkgname) {
  ggplot2_ns <- tryCatch(asNamespace("ggplot2"), error = function(e) NULL)

  has_s7_ggplot2 <- !is.null(ggplot2_ns) &&
    exists("update_ggplot", envir = ggplot2_ns, inherits = FALSE) &&
    exists("class_ggplot", envir = ggplot2_ns, inherits = FALSE) &&
    requireNamespace("S7", quietly = TRUE)

  if (has_s7_ggplot2) {
    # Looked up dynamically (rather than via `ggplot2::update_ggplot` /
    # `ggplot2::class_ggplot` directly) so this still installs and passes
    # `R CMD check` on older ggplot2 (< 4.0), where neither symbol exists --
    # a static `pkg::symbol` reference would be flagged there even though
    # this branch never runs on that version.
    update_ggplot_generic <- get("update_ggplot", envir = ggplot2_ns)
    class_ggplot <- get("class_ggplot", envir = ggplot2_ns)

    S7::method(update_ggplot_generic,
               list(S7::new_S3_class("mosaicresid_layers"), class_ggplot)) <-
      function(object, plot, ...) {
        add_mosaicresid_layers(object, plot)
      }
  }
}

#' Fortify a table object for ggplot2
#'
#' Registers a method so `ggplot(some_table)` works directly on a base R
#' `table` (e.g. `Titanic`, `HairEyeColor`, `UCBAdmissions`) -- matching how
#' base R's own `mosaicplot()` accepts a table as `x`. Without this method,
#' `ggplot()` rejects a `table` object outright, since `ggplot2` only
#' fortifies objects it has a registered method for.
#'
#' This is intentionally just `as.data.frame()`: a `table`'s cell values
#' already *are* the counts, so converting always produces one column per
#' table dimension plus a literal `Freq` count column. See
#' [geom_mosaic_resid()] for how that `Freq` column is then used
#' automatically as an implicit weight.
#'
#' @param model A `table` object.
#' @param data Unused; present for compatibility with the `fortify()` generic.
#' @param ... Unused.
#' @return A data frame with one row per table cell.
#' @importFrom ggplot2 fortify
#' @method fortify table
#' @export
fortify.table <- function(model, data, ...) {
  as.data.frame(model, stringsAsFactors = TRUE)
}

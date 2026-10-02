#' Convert raw or pre-aggregated data into long-format counts
#'
#' Accepts either one-row-per-observation data or data already aggregated
#' with an existing count column (via `weight`), and returns a single tidy
#' long-format count table either way -- the same code path is used
#' downstream regardless of which kind of input was supplied.
#'
#' NA handling matches base R's `table()`/`mosaicplot()` default
#' (`useNA = "no"`): rows with `NA` in either grouping variable are silently
#' excluded, not counted as their own category. Unused factor levels are
#' dropped so phantom zero-total categories never enter the partitioning
#' step.
#'
#' @param data A data frame -- either one row per observation, or already
#'   aggregated with an existing count/weight column.
#' @param x,y Strings naming the two grouping variables.
#' @param weight Optional string naming an existing count column. Leave
#'   `NULL` if `data` is one-row-per-observation.
#' @return A tibble with columns `x`, `y`, and `n` (named after the values of
#'   the `x`/`y` arguments, plus a literal `n` count column).
#' @export
#' @examples
#' df <- data.frame(
#'   genre = sample(c("Action", "Sports", "Puzzle"), 200, replace = TRUE),
#'   platform = sample(c("PS", "Switch"), 200, replace = TRUE)
#' )
#' to_long_counts(df, "genre", "platform")
to_long_counts <- function(data, x, y, weight = NULL) {

  count_var <- "..mosaic_n.."

  if (count_var %in% c(x, y)) {
    stop(
      "Column name '", count_var, "' is reserved for internal use by this ",
      "package and cannot be used as `x` or `y`.",
      call. = FALSE
    )
  }

  # match base R's table()/mosaicplot() default (useNA = "no"): rows with
  # NA in either grouping variable are silently excluded, not counted as
  # their own category
  data <- data[!is.na(data[[x]]) & !is.na(data[[y]]), , drop = FALSE]

  # drop unused factor levels (or coerce non-factor types to character)
  # so phantom zero-total categories never enter the count/partition step
  data[[x]] <- droplevels(as.factor(data[[x]]))
  data[[y]] <- droplevels(as.factor(data[[y]]))

  # IMPORTANT: branch on `weight` in plain R code, outside the tidy-eval
  # call. dplyr::count()'s `wt` argument decides "no weighting" by checking
  # whether the *expression itself* is the literal symbol NULL -- not by
  # evaluating it and checking the result. An `if/else` expression that
  # merely evaluates to NULL is NOT recognized as "no weight," so dplyr
  # falls through to summing an (empty) weight vector, giving 0 for every
  # row. Passing a truly bare `NULL` (by omitting `wt` entirely) or a real
  # column reference are the only two cases that behave correctly.
  if (is.null(weight)) {
    result <- dplyr::count(data, .data[[x]], .data[[y]], name = count_var)
  } else {
    result <- dplyr::count(data, .data[[x]], .data[[y]], wt = .data[[weight]], name = count_var)
  }

  names(result)[names(result) == count_var] <- "n"
  result
}

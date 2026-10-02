#' Add two-way independence-model residuals to a partitioned cell table
#'
#' Computes expected counts under a two-way independence model
#' (`expected = row_total * col_total / N`) and the residual of each cell
#' against that model, using the same three formulas base R's
#' `mosaicplot(type = )` supports -- taken verbatim from
#' `graphics:::mosaicplot.default` and verified numerically against it (see
#' the package tests).
#'
#' This function intentionally only supports exactly two grouping variables.
#' Past two variables, "expected count" stops having one unambiguous
#' definition -- complete independence, joint independence, conditional
#' independence, and all-two-way-associations are different, non-equivalent
#' loglinear models, each requiring the caller to pick one. Rather than
#' silently computing a plausible-looking but wrong number for an ambiguous
#' case, this function fails loudly and points toward the general-purpose
#' tools (`stats::loglin()`, `glm(family = poisson)`) that *do* let you
#' choose a model explicitly.
#'
#' @param cell_table Output of [partition_rects()] (internal) or any data
#'   frame with one row per `(var1, var2)` cell and a count column `n`.
#' @param var1,var2 Strings naming the two grouping-variable columns in
#'   `cell_table`.
#' @param N Total sample size; defaults to `sum(cell_table$n)`. Override
#'   only if you have a specific reason (e.g. residuals relative to a
#'   different total).
#' @param type Residual formula, matching base R's `mosaicplot(type = )`:
#'   `"pearson"` (default) = `(O-E)/sqrt(E)`; `"deviance"` = signed root of
#'   each cell's contribution to the deviance G^2; `"FT"` = Freeman-Tukey.
#' @return `cell_table` with `row_total`, `col_total`, `expected`, and
#'   `residual` columns added.
#' @noRd
add_independence_residuals <- function(cell_table, var1, var2, N = sum(cell_table$n),
                                        type = c("pearson", "deviance", "FT")) {

  type <- match.arg(type)

  expected_rows <- dplyr::n_distinct(cell_table[[var1]]) * dplyr::n_distinct(cell_table[[var2]])
  if (nrow(cell_table) != expected_rows) {
    stop(
      "add_independence_residuals() only supports a 2-way table. ",
      "cell_table has ", nrow(cell_table), " rows but ", var1, " x ", var2,
      " implies ", expected_rows, " -- looks like more variables are present.\n",
      "n-way residuals aren't implemented -- for now, fit your own loglinear ",
      "model (e.g. stats::loglin() or glm(family = poisson)) and compute ",
      "expected counts / residuals manually.",
      call. = FALSE
    )
  }

  cell_table |>
    dplyr::group_by(.data[[var1]]) |> dplyr::mutate(row_total = sum(.data$n)) |> dplyr::ungroup() |>
    dplyr::group_by(.data[[var2]]) |> dplyr::mutate(col_total = sum(.data$n)) |> dplyr::ungroup() |>
    dplyr::mutate(
      expected = .data$row_total * .data$col_total / N,
      residual = switch(type,
        pearson = (.data$n - .data$expected) / sqrt(.data$expected),
        deviance = {
          tmp <- 2 * (.data$n * log(ifelse(.data$n == 0, 1, .data$n / .data$expected)) -
                        (.data$n - .data$expected))
          tmp <- sqrt(pmax(tmp, 0))
          ifelse(.data$n > .data$expected, tmp, -tmp)
        },
        FT = sqrt(.data$n) + sqrt(.data$n + 1) - sqrt(4 * .data$expected + 1)
      )
    )
}

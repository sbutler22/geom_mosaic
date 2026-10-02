#' Carve a sub-rectangle out of a parent rectangle
#'
#' @param bounds Named numeric vector: `xmin`, `xmax`, `ymin`, `ymax`.
#' @param prop Proportion of the parent rectangle this slice occupies (0-1).
#' @param offset Cumulative proportion of prior slices (0-1) -- where this
#'   slice starts within the parent.
#' @param direction `"v"` = split left-to-right (varies x, y unchanged),
#'   `"h"` = split bottom-to-top (varies y, x unchanged).
#' @return A named numeric vector `xmin`/`xmax`/`ymin`/`ymax` for the slice.
#' @noRd
carve <- function(bounds, prop, offset, direction) {
  if (direction == "v") {
    width <- bounds["xmax"] - bounds["xmin"]
    c(
      xmin = unname(bounds["xmin"] + offset * width),
      xmax = unname(bounds["xmin"] + (offset + prop) * width),
      ymin = unname(bounds["ymin"]),
      ymax = unname(bounds["ymax"])
    )
  } else {
    height <- bounds["ymax"] - bounds["ymin"]
    c(
      xmin = unname(bounds["xmin"]),
      xmax = unname(bounds["xmax"]),
      ymin = unname(bounds["ymin"] + offset * height),
      ymax = unname(bounds["ymin"] + (offset + prop) * height)
    )
  }
}

#' Convert sibling proportions into gap-adjusted [start, width] extents
#'
#' @param props Numeric vector of proportions summing to 1.
#' @param spacing Gap size (as a fraction of the parent's dimension) inserted
#'   between each pair of adjacent siblings.
#' @return A tibble with columns `start` and `width`, one row per element of
#'   `props`.
#' @noRd
compute_slice_extents <- function(props, spacing = 0) {
  n <- length(props)
  total_gap <- spacing * (n - 1)
  avail <- 1 - total_gap

  if (avail <= 0) {
    stop(
      "`spacing` is too large for ", n, " categories at this split -- ",
      "gaps alone would consume the whole rectangle. Reduce `spacing`.",
      call. = FALSE
    )
  }

  widths <- avail * props
  starts <- cumsum(c(0, widths[-n])) + spacing * (seq_len(n) - 1)
  tibble::tibble(start = starts, width = widths)
}

#' Recursively partition a rectangle by successive conditional splits
#'
#' The core mosaic-plot geometry: the unit square is split by the first
#' variable's marginal proportions, then each resulting rectangle is split
#' by the next variable's proportions *conditional on* the categories chosen
#' so far, and so on. Area therefore always equals joint proportion
#' (width x height = count / total), exactly matching true mosaic-plot
#' construction rather than a grid-of-equal-size-cells approximation.
#'
#' @param data A data frame, one row per observation (or pre-aggregated with
#'   `weight`).
#' @param vars Character vector of variable names to split on, in order.
#' @param weight Optional string naming a count/weight column; if `NULL`,
#'   each row counts as 1.
#' @param bounds Named numeric vector `xmin`/`xmax`/`ymin`/`ymax` for the
#'   current rectangle.
#' @param level Current recursion depth (used to alternate split direction
#'   when `dir` is not supplied).
#' @param labels Named list accumulating the category chosen at each prior
#'   level.
#' @param spacing Gap size (fraction of each split's dimension) between
#'   sibling rectangles at every level. `0` = no gap.
#' @param dir Optional character vector of `"v"`/`"h"`, one per variable in
#'   the original `vars` list passed to the top-level call (recycled if
#'   shorter). Matches base R's `mosaicplot(dir = )`: lets the caller force
#'   the split direction at each level instead of the default alternating
#'   v/h. `NULL` (the default) reproduces the original alternating behavior
#'   exactly (`"v"` at level 0, `"h"` at level 1, `"v"` at level 2, ...).
#' @return A tibble with one row per leaf rectangle (one per combination of
#'   `vars`' categories), columns `xmin`/`xmax`/`ymin`/`ymax`, `n`, `level`,
#'   and one column per variable in `vars` holding the category chosen at
#'   that level.
#' @noRd
partition_rects <- function(data, vars, weight = NULL,
                             bounds = c(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
                             level = 0, labels = list(), spacing = 0.01,
                             dir = NULL) {

  wt_vec <- if (is.null(weight)) rep(1, nrow(data)) else data[[weight]]

  if (length(vars) == 0) {
    out <- tibble::as_tibble(as.list(bounds))
    out$n <- sum(wt_vec)
    out$level <- level
    for (nm in names(labels)) out[[nm]] <- labels[[nm]]
    return(out)
  }

  this_var <- vars[1]

  splits <- data |>
    dplyr::mutate(.weight__ = wt_vec) |>
    dplyr::count(.data[[this_var]], wt = .data$.weight__, name = "n") |>
    dplyr::mutate(prop = .data$n / sum(.data$n))

  extents <- compute_slice_extents(splits$prop, spacing)
  splits$offset <- extents$start
  splits$prop   <- extents$width   # gap-adjusted width replaces raw proportion

  direction <- if (!is.null(dir)) {
    dir[[(level %% length(dir)) + 1]]
  } else {
    if (level %% 2 == 0) "v" else "h"
  }

  purrr::map_dfr(seq_len(nrow(splits)), function(i) {
    sub_bounds <- carve(bounds, splits$prop[i], splits$offset[i], direction)
    cat_val    <- splits[[this_var]][i]
    sub_data <- data[data[[this_var]] == cat_val, , drop = FALSE]
    sub_labels <- labels
    sub_labels[[this_var]] <- cat_val
    partition_rects(sub_data, vars[-1], weight, sub_bounds, level + 1, sub_labels, spacing, dir)
  })
}

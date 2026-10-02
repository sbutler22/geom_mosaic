make_fixture <- function() {
  set.seed(42)
  n <- 600
  data.frame(
    cat1 = sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    cat2 = sample(c("X", "Y"), n, replace = TRUE, prob = c(0.6, 0.4))
  )
}

test_that("tile widths/heights are non-negative and areas never exceed the unit square", {
  df <- make_fixture()
  rects <- compute_mosaic_residuals(df, "cat1", "cat2", spacing = 0.02)

  w <- rects$xmax - rects$xmin
  h <- rects$ymax - rects$ymin
  expect_true(all(w >= 0))
  expect_true(all(h >= 0))
  expect_true(all(rects$xmin >= -1e-9 & rects$xmax <= 1 + 1e-9))
  expect_true(all(rects$ymin >= -1e-9 & rects$ymax <= 1 + 1e-9))
})

test_that("`spacing = 0` reproduces exact joint proportions (area == n / N)", {
  df <- make_fixture()
  rects <- compute_mosaic_residuals(df, "cat1", "cat2", spacing = 0)
  area <- (rects$xmax - rects$xmin) * (rects$ymax - rects$ymin)
  expect_equal(area, rects$n / sum(rects$n), tolerance = 1e-9)
})

test_that("`dir = NULL` (default) is numerically identical to explicit c('v','h')", {
  df <- make_fixture()
  r_default <- compute_mosaic_residuals(df, "cat1", "cat2", spacing = 0.01)
  r_vh <- compute_mosaic_residuals(df, "cat1", "cat2", spacing = 0.01, dir = c("v", "h"))

  r_default <- r_default[order(r_default$cat1, r_default$cat2), ]
  r_vh <- r_vh[order(r_vh$cat1, r_vh$cat2), ]

  expect_equal(
    r_default[, c("xmin", "xmax", "ymin", "ymax")],
    r_vh[, c("xmin", "xmax", "ymin", "ymax")]
  )
})

test_that("`dir = c('h','v')` swaps split roles: outer variable now stacks by y, not x", {
  df <- make_fixture()
  r_hv <- compute_mosaic_residuals(df, "cat1", "cat2", spacing = 0.01, dir = c("h", "v"))

  bands <- dplyr::summarise(
    dplyr::group_by(r_hv, cat1),
    ymin = min(ymin), ymax = max(ymax), .groups = "drop"
  )
  bands <- bands[order(bands$ymin), ]

  # each top-level category should occupy a distinct, non-overlapping y-band
  expect_true(all(bands$ymax[-nrow(bands)] <= bands$ymin[-1] + 1e-9))
})

test_that("`spacing` too large for the number of categories errors instead of producing garbage", {
  df <- make_fixture()
  expect_error(
    compute_mosaic_residuals(df, "cat1", "cat2", spacing = 0.9),
    "spacing"
  )
})

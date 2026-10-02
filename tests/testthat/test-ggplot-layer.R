make_fixture <- function() {
  set.seed(42)
  n <- 600
  data.frame(
    cat1 = sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    cat2 = sample(c("X", "Y"), n, replace = TRUE, prob = c(0.6, 0.4))
  )
}

test_that("geom_mosaic_residual() builds without error and draws one rect per cell", {
  df <- make_fixture()
  p <- ggplot2::ggplot(df) +
    geom_mosaic_residual(ggplot2::aes(x = cat1, y = cat2), spacing = 0.015)

  built <- ggplot2::ggplot_build(p)
  rect_data <- built$data[[1]]

  expect_equal(nrow(rect_data), dplyr::n_distinct(df$cat1) * dplyr::n_distinct(df$cat2))
  expect_false(any(is.na(rect_data$fill)))
})

test_that("label layer is present by default and absent when label = FALSE", {
  df <- make_fixture()

  p_with_labels <- ggplot2::ggplot(df) +
    geom_mosaic_residual(ggplot2::aes(x = cat1, y = cat2))
  classes_with <- vapply(p_with_labels$layers, function(l) class(l$geom)[1], character(1))

  p_no_labels <- ggplot2::ggplot(df) +
    geom_mosaic_residual(ggplot2::aes(x = cat1, y = cat2), label = FALSE)
  classes_without <- vapply(p_no_labels$layers, function(l) class(l$geom)[1], character(1))

  expect_true("GeomText" %in% classes_with)
  expect_false("GeomText" %in% classes_without)
})

test_that("default scale is replaced cleanly when the user supplies their own", {
  df <- make_fixture()
  p <- ggplot2::ggplot(df) +
    geom_mosaic_residual(ggplot2::aes(x = cat1, y = cat2)) +
    ggplot2::scale_fill_gradient2(low = "purple", mid = "white", high = "darkgreen")

  # labels should still be present after overriding the scale
  classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomText" %in% classes)

  built <- ggplot2::ggplot_build(p)
  expect_false(any(is.na(built$data[[1]]$fill)))
})

test_that("dir and type overrides pass through geom_mosaic_residual() without error", {
  df <- make_fixture()
  p <- ggplot2::ggplot(df) +
    geom_mosaic_residual(ggplot2::aes(x = cat1, y = cat2), dir = c("h", "v"), type = "deviance")

  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$data[[1]]), dplyr::n_distinct(df$cat1) * dplyr::n_distinct(df$cat2))
})

test_that("faceting computes an independent contingency table per panel", {
  df <- make_fixture()
  df$facet <- sample(c("g1", "g2"), nrow(df), replace = TRUE)

  p <- ggplot2::ggplot(df) +
    geom_mosaic_residual(ggplot2::aes(x = cat1, y = cat2)) +
    ggplot2::facet_wrap(~facet)

  built <- ggplot2::ggplot_build(p)
  rect_data <- built$data[[1]]
  expect_equal(length(unique(rect_data$PANEL)), 2)
})

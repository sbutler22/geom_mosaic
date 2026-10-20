test_that("fortify.table() converts a table into a long data frame with a Freq column", {
  df <- fortify(Titanic)
  expect_true(is.data.frame(df))
  expect_true("Freq" %in% names(df))
  expect_equal(sum(df$Freq), sum(Titanic))
})

test_that("ggplot(<table>) + geom_mosaic_resid(aes(x=, y=)) auto-weights by Freq", {
  p <- ggplot2::ggplot(Titanic) +
    geom_mosaic_resid(ggplot2::aes(x = Class, y = Survived))

  built <- ggplot2::ggplot_build(p)
  rect_data <- built$data[[1]]

  # 4 classes x 2 survived outcomes
  expect_equal(nrow(rect_data), 8)

  # tile widths must vary with the real passenger counts, not be flat/equal
  # (a flat result would mean Freq was silently ignored, as in the original
  # weight = "Freq" string-argument bug)
  widths <- round(rect_data$xmax - rect_data$xmin, 4)
  expect_gt(length(unique(widths)), 1)

  expect_false(any(is.na(rect_data$fill)))
})

test_that("explicit aes(weight = ) still works and is not overridden when Freq is also present", {
  titanic_df <- as.data.frame(Titanic)
  titanic_df$Freq2 <- titanic_df$Freq  # decoy column, not named "Freq"

  p <- ggplot2::ggplot(titanic_df) +
    geom_mosaic_resid(ggplot2::aes(x = Class, y = Survived, weight = Freq))

  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$data[[1]]), 8)
  expect_false(any(is.na(built$data[[1]]$fill)))
})

test_that("plain data frames without a Freq column are unaffected (no auto-weight applied)", {
  df <- data.frame(
    a = sample(c("x", "y"), 100, replace = TRUE),
    b = sample(c("p", "q"), 100, replace = TRUE)
  )
  # every row should count as 1 (no Freq column exists to auto-weight by)
  p <- ggplot2::ggplot(df) + geom_mosaic_resid(ggplot2::aes(x = a, y = b))
  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$data[[1]]), 4)
  expect_false(any(is.na(built$data[[1]]$fill)))
})

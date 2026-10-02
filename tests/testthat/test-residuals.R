# Fixture: deterministic synthetic data with built-in (not download-dependent)
# categorical structure, so these tests never need network access.
make_fixture <- function() {
  set.seed(42)
  n <- 600
  data.frame(
    cat1 = sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    cat2 = sample(c("X", "Y"), n, replace = TRUE, prob = c(0.6, 0.4))
  )
}

test_that("pearson residuals match stats::loglin() to floating-point precision", {
  df <- make_fixture()
  tbl <- table(df$cat1, df$cat2)

  ll <- stats::loglin(tbl, margin = list(1, 2), fit = TRUE, print = FALSE)
  E <- ll$fit
  base_pearson <- (tbl - E) / sqrt(E)

  ours <- compute_mosaic_residuals(df, "cat1", "cat2", type = "pearson")

  diffs <- vapply(seq_len(nrow(ours)), function(i) {
    g <- as.character(ours$cat1[i]); p <- as.character(ours$cat2[i])
    ours$residual[i] - base_pearson[g, p]
  }, numeric(1))

  expect_lt(max(abs(diffs)), 1e-9)
})

test_that("deviance residuals match base R's mosaicplot.default formula", {
  df <- make_fixture()
  tbl <- table(df$cat1, df$cat2)

  ll <- stats::loglin(tbl, margin = list(1, 2), fit = TRUE, print = FALSE)
  E <- ll$fit
  O <- tbl
  tmp <- 2 * (O * log(ifelse(O == 0, 1, O / E)) - (O - E))
  tmp <- sqrt(pmax(tmp, 0))
  base_deviance <- ifelse(O > E, tmp, -tmp)

  ours <- compute_mosaic_residuals(df, "cat1", "cat2", type = "deviance")

  diffs <- vapply(seq_len(nrow(ours)), function(i) {
    g <- as.character(ours$cat1[i]); p <- as.character(ours$cat2[i])
    ours$residual[i] - base_deviance[g, p]
  }, numeric(1))

  expect_lt(max(abs(diffs)), 1e-9)
})

test_that("Freeman-Tukey residuals match base R's mosaicplot.default formula", {
  df <- make_fixture()
  tbl <- table(df$cat1, df$cat2)

  ll <- stats::loglin(tbl, margin = list(1, 2), fit = TRUE, print = FALSE)
  E <- ll$fit
  O <- tbl
  base_FT <- sqrt(O) + sqrt(O + 1) - sqrt(4 * E + 1)

  ours <- compute_mosaic_residuals(df, "cat1", "cat2", type = "FT")

  diffs <- vapply(seq_len(nrow(ours)), function(i) {
    g <- as.character(ours$cat1[i]); p <- as.character(ours$cat2[i])
    ours$residual[i] - base_FT[g, p]
  }, numeric(1))

  expect_lt(max(abs(diffs)), 1e-9)
})

test_that("an invalid `type` errors via match.arg", {
  df <- make_fixture()
  expect_error(compute_mosaic_residuals(df, "cat1", "cat2", type = "bogus"))
})

test_that("more than two variables fails loudly rather than computing silently", {
  rects_3way <- data.frame(
    a = rep(c("a1", "a2"), each = 4),
    b = rep(c("b1", "b2"), each = 2, times = 2),
    c = rep(c("c1", "c2"), times = 4),
    n = 1:8
  )
  expect_error(
    add_independence_residuals(rects_3way, "a", "b"),
    "only supports a 2-way table"
  )
})

test_that("weighted (pre-aggregated) input matches raw one-row-per-observation input", {
  df <- make_fixture()
  agg <- dplyr::count(df, cat1, cat2, name = "n")

  raw_result <- compute_mosaic_residuals(df, "cat1", "cat2")
  agg_result <- compute_mosaic_residuals(agg, "cat1", "cat2", weight = "n")

  raw_sorted <- raw_result[order(raw_result$cat1, raw_result$cat2), ]
  agg_sorted <- agg_result[order(agg_result$cat1, agg_result$cat2), ]

  expect_equal(raw_sorted$n, agg_sorted$n)
  expect_equal(raw_sorted$residual, agg_sorted$residual)
})

test_that("NA rows are silently dropped, matching table()'s useNA = 'no' default", {
  df <- make_fixture()
  df$cat1[1:10] <- NA
  expect_equal(nrow(to_long_counts(df, "cat1", "cat2")) > 0, TRUE)
  expect_equal(sum(to_long_counts(df, "cat1", "cat2")$n), nrow(df) - 10)
})

test_that("`..mosaic_n..` as a variable name is rejected with a clear error", {
  df <- make_fixture()
  names(df)[1] <- "..mosaic_n.."
  expect_error(to_long_counts(df, "..mosaic_n..", "cat2"), "reserved")
})

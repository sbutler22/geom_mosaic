# mosaicresid

A `ggplot2`-native alternative to base R's `mosaicplot(shade = TRUE)`.

```r
library(mosaicresid)
library(ggplot2)

ggplot(Titanic) +
  geom_mosaic_resid(aes(x = Class, y = Survived)) +
  theme_minimal()
```

`geom_mosaic_resid()` takes two categorical variables mapped the usual
`ggplot2` way, `aes(x =, y =)`, and automatically:

- Recursively partitions the unit square into tiles sized by joint
  proportion -- true mosaic-plot geometry, not a grid-of-equal-cells
  approximation.
- Computes Pearson (or deviance, or Freeman-Tukey) residuals against a
  two-way independence model.
- Maps the residual to fill color via a diverging scale.
- Draws in-tile category labels (mosaic plots can't use ordinary axis
  ticks for the inner variable, since its split positions differ from
  column to column).

Works on raw, one-row-per-observation data or already-aggregated
contingency-table data through the same code path, and the underlying
tile geometry and residuals are available directly as a tidy data frame
via `compute_mosaic_residuals()`.

## Installation

```r
# development version from GitHub
# install.packages("remotes")
remotes::install_github("sbutler22/geom_mosaic")
```

## Documentation

- `vignette("getting-started", package = "mosaicresid")`
- `vignette("comparison", package = "mosaicresid")` -- an honest comparison
  against base R's `mosaicplot()` and the `ggmosaic2` package, including
  what this package deliberately does and does not implement.

## License

MIT

## Use of AI

This package was developed with substantial help from Claude (Anthropic),
used as a coding assistant. Claude drafted much of the R code, roxygen
documentation, tests, and vignette text, and helped debug issues such as
compatibility with ggplot2 4.0. I directed the design (including the
`aes(x =, y =)` interface, the two-variable scope, and the default color
scale), reviewed and tested the code, chose the example datasets, and
edited the documentation. The residual calculations are checked against
base R's own `mosaicplot()` formulas in the package's test suite.

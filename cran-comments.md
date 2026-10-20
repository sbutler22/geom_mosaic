## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Test environments

* local macOS (aarch64), R 4.6.1
* win-builder (R-devel)

## Notes

* `S7` is listed in Suggests and is used only on ggplot2 >= 4.0, where it
  is already installed as a dependency of ggplot2. It registers this
  package's plot-addition method with ggplot2's S7 `update_ggplot()`
  generic; on older ggplot2 the classic S3 `ggplot_add()` method is used
  instead.
## Test environments
* local Windows 11 x64, R 4.5.1
* win-builder (R-devel, R-release)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time
  (Standard local Windows platform check note; all files have valid timestamps)

## Submission summary

This is an update release (v1.0.3) of the 'immunaut' package introducing structural improvements and bug fixes:
* Enforced strict machine learning isolation in `auto_simon_ml()` by performing the train/test partition prior to any preprocessing transformations, eliminating data leakage.
* Added export of fitted preprocessing parameters (`preProcessParams`) from training data to allow identical transformation of independent external validation cohorts.
* Fixed multi-class Macro-F1 score calculation across class levels via `confusionMatrix$byClass`.
* Vectorized candidate metric normalization across partitions in `pick_best_cluster_overall()`.
* Hardened internal helper functions (`normalize()`, `pick_best_cluster_modularity()`, `pick_best_cluster_silhouette()`) against missing values.
* Added fallback parameter defaults in `immunaut()` to handle empty or unspecified settings cleanly.
* Added comprehensive unit test suite in `tests/testthat/` with 43 passing assertions.
* Updated documentation, examples, and README.

## Downstream dependencies
There are currently no reverse dependencies for this package.

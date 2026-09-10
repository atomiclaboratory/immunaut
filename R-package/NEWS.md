# immunaut 1.0.3

- **Machine Learning Data Leakage Fix**: Enforced strict train/test data isolation in `auto_simon_ml()`. The dataset is partitioned into training and testing sets *prior* to any preprocessing transformations.
- **Preprocessing Isolation**: `preProcessData()` and `preProcessResample()` estimate imputation, centering, scaling, and zero-variance filtering parameters exclusively on the training partition and apply them to the test set without information leakage.
- **Fitted Preprocessing Object**: Added `preProcessParams` to the `auto_simon_ml()` output object for reproducible transformations on external validation datasets.
- **Multi-Class Evaluation**: Added `MLmetrics` to `Imports` and fixed multi-class Macro-F1 score calculation to correctly extract class-level metrics from `confusionMatrix$byClass`.
- **Vectorized Metric Normalization in `pick_best_cluster_overall()`**: Resolved a scalar normalization bug where candidate cluster scores were collapsed to 0.5. Candidate metrics (modularity, silhouette, Davies-Bouldin index, Calinski-Harabasz index) are now normalized across all candidate partitions simultaneously, with index alignment preserved.
- **Robust NA Handling in `normalize()`**: Handled `NA`s gracefully via `range(..., na.rm = TRUE)` to prevent `missing value where TRUE/FALSE needed` errors on incomplete vectors.
- **Safe Comparison in `pick_best_cluster_modularity()` & `pick_best_cluster_silhouette()`**: Added `!is.na()` checks to ensure clustering selection does not crash when candidates contain missing metric values.
- **Clean Default Handling in `immunaut()`**: Automatically generates `fileHeader` if NULL and defaults `selectedColumns` safely to avoid dropping all features.
- **Preserved Rows in `immunaut(..., removeNA = TRUE)`**: Properly aligned `dataset_ml` rows with filtered observations so that clustering labels are retained even after `na.omit()` drops incomplete cases.
- **Adaptive t-SNE and Louvain Parameter Clamping**:
  - `calculate_tsne()` respects user-specified seeds (falling back to 1337) and dynamically clamps `perplexity` to the maximum mathematical threshold allowed by sample size (`(n - 1) / 3`).
  - Safe zero-variance feature filtering using `isTRUE(stats::var(., na.rm = TRUE) > 0)` prevents tidyselect predicate evaluation crashes on all-NA or constant columns.
  - `cluster_tsne_knn_louvain()` dynamically adjusts `knn_clusters` whenever $k \ge n$ to prevent `FNN::get.knn` crashes and graph construction failures.
- **DBSCAN and Outlier Factor Alignment**: DBSCAN noise points (labeled 0 or NA) in `cluster_tsne_density()`, `cluster_tsne_mclust()`, and `cluster_tsne_hierarchical()` are properly mapped to cluster "100" with factor levels explicitly updated, eliminating accidental `NA` conversions.
- **Plotting Improvements in `plot_clustered_tsne()`**:
  - Eliminated global R session modification caused by `theme_set()`, applying themes locally to the `ggplot` object.
  - Preserves non-numeric/string cluster identifiers as valid factors instead of coercing them to `NA` via `as.numeric()`.
  - Removed stray debug `print()` statements in `is_var_empty()`.
- **Comprehensive Unit Testing**: Added a test suite (`test-auto_simon_ml.R` and `test-pipeline_and_fixes.R`) with 43 assertions covering ML isolation, clustering methods, edge cases, NA robustness, and visualization.

# immunaut 1.0.2

- **Add datasets**: Added new datasets - https://zenodo.org/records/14719593
- **Add examples**: Added examples for new datasets



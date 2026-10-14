# ScreenR 1.15.5

## More general input

* `create_screenr_object()` accepts tables with any column names:
  `barcode_column` and `gene_column` give the names of the barcode and gene
  columns, which are renamed internally to `Barcode` and `Gene`.
  `get_column_names()` returns the original names. When a column is not found
  the error suggests the most similar column and the argument to use.
* The Day and Treatment of each sample can be given with `sample_table` (and
  `sample_columns` for its column names) instead of relying on sample names of
  the form `Day_Treatment_Replicate`. `get_sample_table()` shows how the
  samples are interpreted; a message is given when sample names do not follow
  the convention.
* The input is checked when the object is created: `groups` must have one
  value for each sample; sample columns stored as text are converted to
  numbers; other non-numeric columns of the count table are removed with a
  message; duplicated barcodes and barcodes missing from the annotation give
  a warning; duplicated barcodes in the annotation are reduced to their first
  row.
* The annotation columns `Sequence`, `Library` and `Gene_ID` are now optional.
* New `show()` method, so that objects saved with older versions of ScreenR
  can still be printed.

## New features

* `plot_gini()` plots the Gini index computed by `compute_gini()`.

# ScreenR 1.15.4

## Bug fixes in the vignette

* The design matrix of the example analysis was built with
  `model.matrix(~ groups)` and its columns renamed with `unique(groups)`,
  which does not match the column order: the contrast "Time3_TRT" actually
  compared the Time3 control samples with the baseline. The vignette now uses
  `create_design_matrix()` and `create_contrast()` to compare treated and
  control samples of the same time point.
* The "at least 2 methods" common hits used `roast_hit_TRT$Day3` and `$Day6`,
  which do not exist, so ROAST was ignored.

## Changes that affect the results

* `find_camera_hit()` gains `fdr = 0.05` and returns only the genes with
  FDR <= fdr (previously all the genes with the requested direction). Use
  `fdr = 1` for the previous behaviour.
* `find_roast_hit()` gains `fdr = 0.05` and filters on the FDR; the raw p-value
  cut off `p_val` is now optional (default `NULL`, previously 0.05).
* `compute_metrics()` gains `pseudocount = 0.5`: the log2 fold change is
  `log2((Treated + pseudocount) / (Control + pseudocount))` instead of
  `log2(Treated / Control + 1e-7)`, and only barcodes with zero reads in both
  treated and control are removed (previously all barcodes with zero control
  reads).

## New features

* `create_design_matrix()` and `create_contrast()` build the design matrix and
  the contrasts by group name. `find_camera_hit()`, `find_roast_hit()`,
  `plot_barcode_hit()` and `filter_by_variance()` accept a column name, a
  contrast vector or a `limma::makeContrasts()` matrix.
* `find_camera_hit()` and `find_roast_hit()` report `Log2FC`, the median log2
  fold change of the barcodes of each gene; `find_roast_hit()` gains `seed`.
* `normalize_data()` gains `method = "tmm"` (edgeR TMM factors) and
  `method = "control"` (normalization to control barcodes).
* `find_gene_zscore_hit()`: gene-level binomial test with FDR for the Z-score
  method.
* `compute_barcode_statistics()`: replicate-aware barcode statistics with the
  edgeR quasi-likelihood F-test.
* `find_time_hit()`: one model for the whole time course, testing whether the
  treatment effect changes over time.
* `summarise_hits()`: combines the FDR of several methods (MaxFDR,
  intersection-union test).
* Quality control: `plot_replicate_correlation()`, `compute_gini()` and
  `find_discordant_barcodes()`.
* New vignette "Improved statistical analysis with ScreenR".
* edgeR >= 4.0.0 is now required.

# ScreenR 1.15.3

## Changes

* `find_zscore_hit()` now checks the `metric` argument (`"median"`, the
  default, or `"mean"`) and gives a clear error for any other value.
* `find_robust_zscore_hit()` now has a default `number_barcode = 6`, the same
  as `find_zscore_hit()`, and returns an ungrouped tibble like the other hit
  functions.
* The `thresh` and `lfc` arguments of `find_camera_hit()` are deprecated. They
  never had an effect on the result; passing them now gives a warning.
  Filter the returned table on its `FDR` column instead.
* Removed the stray `README.knit.md` file from the package.
* Added tests for these changes and for the 1.15.2 bug fixes.

# ScreenR 1.15.2

## Bug fixes (these change the results of some analyses)

* `create_edger_obj()` now builds the edgeR object from the raw counts
  instead of the CPM-normalized counts, so edgeR uses the real library sizes.
  The annotation is matched to the counts by `Barcode`, and an error is raised
  when barcodes are missing from the annotation. This affects
  `find_camera_hit()`, `find_roast_hit()`, `filter_by_variance()` and
  `plot_barcode_hit()`.
* The `number_barcode` argument of `find_camera_hit()` and `find_roast_hit()`
  is now applied: only genes with more than `number_barcode` barcodes are
  tested. Previously every gene was tested.
* `compute_metrics()` now computes the standard robust Z-score,
  `(Log2FC - median(Log2FC)) / mad(Log2FC)`. This affects
  `find_robust_zscore_hit()`.
* `compute_slope()` (used by `filter_by_slope()`) now regresses frequencies on
  the time points, with replicates sharing the same time point, instead of on
  the row number.
* `plot_barcode_hit()` now uses the design matrix to estimate the dispersion
  and honours its `number_barcode` argument.
* `filter_by_slope()` no longer prints the control slopes.

# ScreenR 1.15.1

* Fixed the vignette build failure with recent edgeR versions (`DPOTRF`
  error code -4 in `find_camera_hit()`).

# ScreenR 0.99.53
* Changes in version 0.99.53:
 * Update to the documentation for the example dataset as requested by the 
   reviewer
 * Pushed to Bioconductor


* Changes in version 0.99.52:
 * Pushed to Bioconductor
 
* Changes in version 0.99.51:
 * Pushed to Bioconductor

* Changes in version 0.99.5:
  * Now ScreenR uses R 4.2 or above
  * Lazy data is set to FALSE
  * In the DESCRIPTION is now present a BugReport link
  * Now all the function are imported using importFrom
  * The vignette has now the BiocStyle
  * All chunks of code of the vignette are set to show warnings
  * Implemented the set and get function for the ScreenR object
  
  
# ScreenR 0.99.41  
* Changes in version 0.99.41:
  * Update to Bioconductor

# ScreenR 0.99.4
* Changes in version 0.99.4:
  * Changed dataframe for the example

# ScreenR 0.99.3
* Changes in version 0.99.3:
  * Fixed some other bug 
  * Changed find_common_hit

# ScreenR 0.99.2
* Changes in version 0.99.2:
  * Fixed some other bug 


# ScreenR 0.99.1
* Changes in version 0.99.1:
  * Fixed some bug 

# ScreenR 0.99.0
* Changes in version 0.99.0:
  * Submitted to Bioconductor 

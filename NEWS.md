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

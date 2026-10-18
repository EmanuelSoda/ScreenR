# Find genes with a consistent directional effect across contrasts

Runs the same gene-level method for every contrast of one design matrix,
then tests for an effect in all contrasts with
[`combine_consistent_hits`](https://emanuelsoda.github.io/ScreenR/reference/combine_consistent_hits.md).

## Usage

``` r
find_consistent_hit(
  screenR_Object,
  matrix_model,
  contrasts,
  method = c("fry", "roast", "camera", "zscore"),
  direction = c("Down", "Up"),
  fdr = 0.05,
  fdr_method = c("BH", "BY"),
  method_args = list()
)
```

## Arguments

- screenR_Object:

  A normalized ScreenR object.

- matrix_model:

  A design matrix with rows in the sample order of the object, with
  sample identifiers as row names. Mismatched names or order are errors
  (see
  [`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md)).
  Use a design appropriate for the experimental dependence.

- contrasts:

  A uniquely named list of at least two contrasts accepted by
  [`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
  such as treatment versus control at each time point. The same design
  and normalization are used throughout.

- method:

  "fry" (default), "roast", "camera", or "zscore". The zscore method
  first uses
  [`compute_barcode_statistics`](https://emanuelsoda.github.io/ScreenR/reference/compute_barcode_statistics.md)
  to obtain replicate-aware barcode Z-scores, then runs
  [`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md).
  Both reference = "random" (its default) and reference = "empirical"
  are supported through method_args.

- direction:

  The prespecified direction, "Down" or "Up".

- fdr:

  Maximum combined FDR to return; 1 returns all tested genes.

- fdr_method:

  "BH" or "BY" for the combined test across genes.

- method_args:

  A named list of additional arguments for the selected method, for
  example list(number_barcode = 3), list(nrot = 9999, seed = 42) for
  ROAST, or list(reference = "empirical", min_barcodes = 4) for Z-score.
  Arguments controlling the object, model, contrast, direction or
  filtering cannot be overridden. Z-score uses score = "Zscore".

## Value

The tibble described in
[`combine_consistent_hits`](https://emanuelsoda.github.io/ScreenR/reference/combine_consistent_hits.md).

## Details

FRY and ROAST test a self-contained effect in every contrast; CAMERA
tests enrichment relative to the other genes in every contrast. The
Z-score interpretation depends on its chosen reference and its
calibration. These are different hypotheses. Legacy barcode-count rules
have no gene-level p-values and are not supported.

All tested genes, including opposite observed directions, are retained
for combination. The existing single-contrast methods keep their usual
defaults. Different gene universes across contrasts are an error. For
precomputed complete p-value tables, use combine_consistent_hits().

## See also

[`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
[`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
[`find_time_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
design <- create_design_matrix(object)
contrasts <- list(
    Treatment = create_contrast(design, "Treated", "Control"),
    Baseline = create_contrast(design, "Treated", "T1/T2")
)
find_consistent_hit(object, design, contrasts, method = "fry")
#> Warning: 3rows with all zero counts
#> Warning: 3rows with all zero counts
#> # A tibble: 0 × 9
#> # ℹ 9 variables: Gene <chr>, Direction <chr>, NContrasts <int>, PValue <dbl>,
#> #   FDR <dbl>, PValue_Treatment <dbl>, Log2FC_Treatment <dbl>,
#> #   PValue_Baseline <dbl>, Log2FC_Baseline <dbl>
```

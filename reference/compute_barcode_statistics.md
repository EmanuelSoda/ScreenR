# Compute replicate-aware barcode statistics

Tests, for each barcode, the comparison given by the contrast with the
edgeR quasi-likelihood F-test
([`glmQLFit`](https://rdrr.io/pkg/edgeR/man/glmQLFit.html),
[`glmQLFTest`](https://rdrr.io/pkg/edgeR/man/glmQLFTest.html)). Unlike
[`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md),
which averages the replicates before computing the log2 fold change,
this uses the variability between replicates: barcodes that are noisy
across replicates get a smaller score. The score is reported as a signed
Z-score (sign(Log2FC) \* qnorm(1 - PValue / 2)), so that the table can
be used with
[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md)
and
[`find_discordant_barcodes`](https://emanuelsoda.github.io/ScreenR/reference/find_discordant_barcodes.md).

## Usage

``` r
compute_barcode_statistics(screenR_Object, matrix_model, contrast)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- matrix_model:

  The design matrix of the linear model, for example created with
  [`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md)
  (or with [`model.matrix`](https://rdrr.io/r/stats/model.matrix.html)).

- contrast:

  The comparison to test: the name or index of a column of matrix_model,
  a numeric contrast vector (for example created with
  [`create_contrast`](https://emanuelsoda.github.io/ScreenR/reference/create_contrast.md))
  or a one-column matrix created with
  [`makeContrasts`](https://rdrr.io/pkg/limma/man/makeContrasts.html).

## Value

A tibble with one row for each barcode and the columns Barcode, Gene,
Log2FC, logCPM, F, PValue, FDR (Benjamini-Hochberg, over all the
barcodes) and Zscore.

## See also

[`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md),
[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
[`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md),
[`create_contrast`](https://emanuelsoda.github.io/ScreenR/reference/create_contrast.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
design <- create_design_matrix(object)
contrast <- create_contrast(design,
    treatment = "Treated",
    control = "Control"
)
statistics <- compute_barcode_statistics(object, design, contrast)
head(statistics)
#> # A tibble: 6 × 8
#>   Barcode Gene      Log2FC logCPM      F  PValue   FDR Zscore
#>   <chr>   <chr>      <dbl>  <dbl>  <dbl>   <dbl> <dbl>  <dbl>
#> 1 10208   Gene_380  0.0346   8.56 0.0756 0.785   0.989  0.273
#> 2 10502   Gene_380 -0.151    4.97 0.129  0.722   0.987 -0.356
#> 3 10796   Gene_380 -0.104    6.61 0.0894 0.767   0.987 -0.297
#> 4 11089   Gene_380 -0.0515   8.44 0.149  0.702   0.983 -0.383
#> 5 11382   Gene_380 -0.621    5.62 2.63   0.114   0.934 -1.58 
#> 6 11675   Gene_380 -0.275    9.66 8.72   0.00573 0.790 -2.76 
```

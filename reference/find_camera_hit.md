# Find hits with CAMERA

Finds the genes whose barcodes are depleted (or enriched) in the
comparison given by the contrast, using the
[CAMERA](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3458527/) method
of Wu and Smyth (2012),
[`camera`](https://rdrr.io/pkg/limma/man/camera.html). CAMERA is a gene
set test: here each gene is a set of barcodes, and the barcodes of the
gene are compared with all the other barcodes (a competitive test).
Unlike
[`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
it also works with a single control sample (for example a plasmid
library).

## Usage

``` r
find_camera_hit(
  screenR_Object,
  matrix_model,
  contrast,
  number_barcode = 3,
  thresh = 1e-04,
  lfc = 1,
  direction = c("Down", "Up"),
  fdr = 0.05,
  fdr_method = c("BH", "storey")
)
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

- number_barcode:

  Only genes with more than number_barcode barcodes are tested (default
  3, so a gene needs at least 4 barcodes). The barcodes of a gene are
  tested together as a set, so this is not a number of barcodes that
  must change.

- thresh:

  Deprecated and ignored: it never had an effect on the result. Filter
  the returned table on the FDR column instead.

- lfc:

  Deprecated and ignored: it never had an effect on the result.

- direction:

  The direction of the change: "Down" (default) for depleted genes or
  "Up" for enriched genes.

- fdr:

  The FDR threshold: only genes with an FDR lower than or equal to this
  value are returned (default 0.05). Use fdr = 1 to return all the genes
  with the given direction.

- fdr_method:

  How the FDR is computed: "BH" (Benjamini-Hochberg, the default) or
  "storey" (Benjamini-Hochberg multiplied by the estimated proportion of
  genes without effect, less conservative when many genes have an
  effect).

## Value

A tibble with one row for each hit in the requested direction and the
columns Gene, NGenes (the number of barcodes of the gene), Direction,
PValue (the one-sided p-value for the requested direction), FDR and
Log2FC (the median log2 fold change of the barcodes of the gene).

## Details

The raw counts are modelled with edgeR (the dispersion is estimated with
[`estimateDisp`](https://rdrr.io/pkg/edgeR/man/estimateDisp.html)), and
camera tests the barcodes of each gene together, accounting for the
correlation between them. The two-sided p-values of camera are converted
into one-sided p-values for the requested direction and the FDR is
computed from them over all the tested genes.

CAMERA is a competitive test: it compares the barcodes of each gene with
all the other barcodes, so when many genes have an effect it becomes
conservative.
[`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md)
with method = "fry" (self-contained) does not have this limitation, and
neither does
[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md)
with control genes as reference, as long as the control genes behave
like genes without effect (see its documentation).

## See also

[`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
[`find_time_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md),
[`summarise_hits`](https://emanuelsoda.github.io/ScreenR/reference/summarise_hits.md),
[`plot_barcode_hit`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md),
[`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md),
[`create_contrast`](https://emanuelsoda.github.io/ScreenR/reference/create_contrast.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

matrix <- create_design_matrix(object)
contrast <- create_contrast(matrix, treatment = "Treated",
    control = "Control")

result <- find_camera_hit(
    screenR_Object = object,
    matrix_model = matrix, contrast = contrast
)
#> Warning: 3rows with all zero counts
#> Warning: Zero sample variances detected, have been offset away from zero
head(result)
#> # A tibble: 1 × 6
#>   Gene     NGenes Direction    PValue     FDR Log2FC
#>   <chr>     <dbl> <fct>         <dbl>   <dbl>  <dbl>
#> 1 Gene_173     10 Down      0.0000160 0.00856 -0.429
```

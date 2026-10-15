# Filter hits by the variance of their barcodes

Refines a list of candidate hits by the consistency of their barcodes.
For each gene it computes the variance of the log2 fold changes of its
barcodes (estimated with edgeR for the given contrast) and keeps the
genes whose variance is at most the given threshold: a gene whose
barcodes disagree is a less reliable hit.

## Usage

``` r
filter_by_variance(
  screenR_Object,
  genes,
  matrix_model,
  variance = 0.5,
  contrast
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- genes:

  The genes to filter, for example the hits found by
  [`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
  [`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)
  or
  [`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md).

- matrix_model:

  The design matrix of the linear model, for example created with
  [`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md)
  (or with [`model.matrix`](https://rdrr.io/r/stats/model.matrix.html)).

- variance:

  The maximum variance of the barcode log2 fold changes accepted
  (default 0.5).

- contrast:

  The comparison used to compute the log2 fold changes: the name or
  index of a column of matrix_model, a numeric contrast vector (for
  example created with
  [`create_contrast`](https://emanuelsoda.github.io/ScreenR/reference/create_contrast.md))
  or a one-column matrix created with
  [`makeContrasts`](https://rdrr.io/pkg/limma/man/makeContrasts.html).

## Value

The rows of the data table (see
[`get_data_table`](https://emanuelsoda.github.io/ScreenR/reference/get_data_table.md))
of the genes that pass the filter, with an additional Variance column
(the variance of the barcode log2 fold changes of the gene).

## See also

[`filter_by_slope`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md),
[`find_discordant_barcodes`](https://emanuelsoda.github.io/ScreenR/reference/find_discordant_barcodes.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
matrix_model <- create_design_matrix(object)
contrast <- create_contrast(matrix_model,
    treatment = "Treated", control = "Control"
)

data <- filter_by_variance(
    screenR_Object = object, genes = c("Gene_42"),
    matrix_model = matrix_model, contrast = contrast
)
head(data)
#> # A tibble: 6 × 10
#>   Barcode Gene    Sample      Frequency Sequence Library Gene_ID Day   Treatment
#>   <fct>   <chr>   <fct>           <dbl> <lgl>    <lgl>   <lgl>   <chr> <chr>    
#> 1 12937   Gene_42 T1               471. NA       NA      NA      T1    T1       
#> 2 12937   Gene_42 T2               406. NA       NA      NA      T2    T2       
#> 3 12937   Gene_42 Time3_TRT_A      365. NA       NA      NA      Time3 TRT      
#> 4 12937   Gene_42 Time3_TRT_B      448. NA       NA      NA      Time3 TRT      
#> 5 12937   Gene_42 Time3_TRT_C      394. NA       NA      NA      Time3 TRT      
#> 6 12937   Gene_42 Time3_A          459. NA       NA      NA      Time3 Time3    
#> # ℹ 1 more variable: Variance <dbl>
```

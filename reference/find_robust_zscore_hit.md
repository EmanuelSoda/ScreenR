# Title Find robust Z-score Hit

Title Find robust Z-score Hit

## Usage

``` r
find_robust_zscore_hit(table_treate_vs_control, number_barcode)
```

## Arguments

- table_treate_vs_control:

  A table computed with the function `compute_data_table`. It contain
  for each barcode the associated Gene the counts in the treated and
  control and the value for the Log2FC, Zscore, ZscoreRobust in each
  day.

- number_barcode:

  Number of barcode that as to be differentially expressed (DE)in order
  to consider the gene associated DE. Example a gene is associated with
  10 shRNA we consider a gene DE if it has at least number_barcode = 5
  shRNA DE.

## Value

Return a tibble containing the hit for the robust Z-score

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
table <- compute_metrics(object,
    control = "TRT", treatment = "Time3",
    day = "Time3"
)
result <- find_robust_zscore_hit(table, number_barcode = 6)
head(result)
#> # A tibble: 6 × 2
#> # Groups:   numberOfBarcode [2]
#>   Gene     numberOfBarcode
#>   <chr>              <int>
#> 1 Gene_102               8
#> 2 Gene_115               7
#> 3 Gene_117               7
#> 4 Gene_124               7
#> 5 Gene_129               7
#> 6 Gene_13                7
```

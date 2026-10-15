# Find hits with the robust Z-score threshold rule

Finds the genes with more than number_barcode barcodes whose robust
Z-score is below (direction = "Down", the default) or above (direction =
"Up") the median of the robust Z-scores of all the barcodes. It is the
same rule as
[`find_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md),
applied to the ZscoreRobust column; like that rule it gives no p-value:
use
[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md)
with score = "ZscoreRobust" for a gene-level test with an FDR.

## Usage

``` r
find_robust_zscore_hit(
  table_treate_vs_control,
  number_barcode = 6,
  direction = c("Down", "Up")
)
```

## Arguments

- table_treate_vs_control:

  The table of barcode metrics returned by
  [`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md),
  with one row for each barcode and the columns Gene and ZscoreRobust.

- number_barcode:

  A gene is a hit when more than number_barcode of its barcodes have a
  robust Z-score below (or above) the median (default 6).

- direction:

  "Down" (default) to find depleted genes (barcodes below the median) or
  "Up" to find enriched genes (barcodes above it).

## Value

An ungrouped tibble with the columns Gene and numberOfBarcode (the
number of barcodes of the gene below, or above, the median), with one
row for each hit.

## See also

[`find_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md),
[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
[`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
table <- compute_metrics(object,
    control = "Time3", treatment = "TRT",
    day = "Time3"
)
result <- find_robust_zscore_hit(table, number_barcode = 6)
head(result)
#> # A tibble: 6 × 2
#>   Gene     numberOfBarcode
#>   <chr>              <int>
#> 1 Gene_1                 7
#> 2 Gene_11                7
#> 3 Gene_116               8
#> 4 Gene_120               8
#> 5 Gene_121               7
#> 6 Gene_128               8
```

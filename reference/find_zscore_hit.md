# Find hits with the Z-score threshold rule

Finds the genes with more than number_barcode barcodes whose Z-score is
below (direction = "Down", the default) or above (direction = "Up") the
median (or the mean) of the Z-scores of all the barcodes. This simple
rule gives no p-value and flags many genes without any effect (with the
median, about 17% of them when genes have 10 barcodes and number_barcode
= 6): use
[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
which computes a p-value and an FDR for each gene, instead.

## Usage

``` r
find_zscore_hit(
  table_treate_vs_control,
  number_barcode = 6,
  metric = c("median", "mean"),
  direction = c("Down", "Up")
)
```

## Arguments

- table_treate_vs_control:

  The table of barcode metrics returned by
  [`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md),
  with one row for each barcode and the columns Gene and Zscore.

- number_barcode:

  A gene is a hit when more than number_barcode of its barcodes have a
  Z-score below the threshold (default 6). For example, with the default
  a gene with 10 barcodes is a hit when at least 7 of them are below the
  threshold.

- metric:

  The threshold on the Z-score: "median" (the default) or "mean" of the
  Z-scores of all the barcodes.

- direction:

  "Down" (default) to find depleted genes (barcodes below the threshold)
  or "Up" to find enriched genes (barcodes above it).

## Value

A tibble with the columns Gene and numberOfBarcode (the number of
barcodes of the gene below, or above, the threshold), with one row for
each hit.

## See also

[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
[`find_robust_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_robust_zscore_hit.md),
[`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md),
[`find_common_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_common_hit.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
table <- compute_metrics(object,
    control = "Time3", treatment = "TRT",
    day = "Time3"
)

# Threshold on the median
result <- find_zscore_hit(table, number_barcode = 6)
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

# Threshold on the mean
result <- find_zscore_hit(table, number_barcode = 6, metric = "mean")
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

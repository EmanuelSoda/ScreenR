# Find discordant barcodes

Finds the barcodes whose log2 fold change is far from that of the other
barcodes of the same gene. Discordant barcodes may have off-target
effects or a poor knockdown, and can drive a gene to be (or not to be) a
hit.

## Usage

``` r
find_discordant_barcodes(table_treate_vs_control, threshold = 3)
```

## Arguments

- table_treate_vs_control:

  A table with one row for each barcode and at least the columns
  Barcode, Gene and Log2FC, as returned by
  [`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
  or
  [`compute_barcode_statistics`](https://emanuelsoda.github.io/ScreenR/reference/compute_barcode_statistics.md).

- threshold:

  The number of MADs from the gene median beyond which a barcode is
  discordant (default 3).

## Value

A tibble with the discordant barcodes and the columns Barcode, Gene,
Log2FC, GeneLog2FC (median of the gene) and Deviation (distance from the
gene median in MADs), ordered by the absolute deviation.

## Details

For each gene it computes the median and the median absolute deviation
(MAD) of the log2 fold changes of its barcodes; a barcode is discordant
when its distance from the median is larger than threshold MADs. When
the MAD of a gene is 0, every barcode that differs from the median is
discordant, with an infinite deviation.

Use it to inspect the hits, not to filter the barcodes before finding
them. The discordant barcodes are found with the same fold changes that
the tests use, so removing them is circular: it makes the genes look
more consistent than they are and the p-values too small. In shRNA
screens it also removes the hairpins that work, when most of the
hairpins of an essential gene do not. In the K562 screen
([`k562_count_table`](https://emanuelsoda.github.io/ScreenR/reference/k562_count_table.md))
removing them before
[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md)
found fewer essential genes and more non-essential ones. To exclude bad
barcodes, use information that does not come from the comparison, for
example barcodes that are discordant in several independent screens with
the same library.

## See also

[`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md),
[`filter_by_variance`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_variance.md),
[`plot_barcode_trend`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_trend.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
table <- compute_metrics(object,
    control = "Time3", treatment = "TRT",
    day = "Time3"
)
find_discordant_barcodes(table)
#> # A tibble: 284 × 5
#>    Barcode Gene     Log2FC GeneLog2FC Deviation
#>    <fct>   <chr>     <dbl>      <dbl>     <dbl>
#>  1 12166   Gene_197  0.707    0.0212       20.8
#>  2 30030   Gene_245 -4.24    -0.0162      -17.3
#>  3 12170   Gene_201 -2.75     0.0292      -14.5
#>  4 11618   Gene_282 -2.23     0.0196      -14.2
#>  5 13414   Gene_44   1.03    -0.123        13.4
#>  6 14060   Gene_235  0.971    0.0642       11.8
#>  7 13186   Gene_62   1.16     0.101        11.8
#>  8 11838   Gene_143 -1.15    -0.0330      -11.6
#>  9 10567   Gene_496  0.766    0.00465      11.4
#> 10 11975   Gene_416 -1.07     0.230       -10.6
#> # ℹ 274 more rows
```

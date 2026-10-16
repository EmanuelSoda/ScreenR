# Plot the empirical null of the gene statistics

Plots the distribution of the gene statistics used by
[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md)
with reference = "empirical", together with the fitted null (scaled to
the share of genes it describes). The null should follow the centre of
the distribution; the hits are in the tail on the side of direction.

## Usage

``` r
plot_empirical_null(
  table_treate_vs_control,
  score = c("Zscore", "ZscoreRobust"),
  direction = c("Down", "Up"),
  statistic = c("mean", "median"),
  min_barcodes = 3,
  empirical_fit = c("central", "one-sided"),
  bins = 60
)
```

## Arguments

- table_treate_vs_control:

  A table with one row for each barcode and at least the columns Gene,
  Log2FC and the score column, as returned by
  [`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
  or
  [`compute_barcode_statistics`](https://emanuelsoda.github.io/ScreenR/reference/compute_barcode_statistics.md).

- score:

  The column to use as score: "Zscore" (default) or "ZscoreRobust".

- direction:

  "Down" (default) or "Up".

- statistic:

  The gene score: "mean" (default) or "median".

- min_barcodes:

  Genes with fewer barcodes than this are not used (default 3).

- empirical_fit:

  "central" (default) or "one-sided".

- bins:

  The number of bins of the histogram. Default 60.

## Value

A ggplot object.

## See also

[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
table <- compute_metrics(object,
    control = "Time3", treatment = "TRT",
    day = "Time3"
)
plot_empirical_null(table)
```

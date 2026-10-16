# Gene statistics and their empirical null

Internal function used by
[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md)
(reference = "empirical") and
[`plot_empirical_null`](https://emanuelsoda.github.io/ScreenR/reference/plot_empirical_null.md).
It computes the score of each gene (mean or median of the scores of its
barcodes), standardizes it into a statistic, fits the empirical null of
the statistics and computes the p-values.

## Usage

``` r
empirical_null_statistics(
  table,
  score = "Zscore",
  direction = "Down",
  statistic = "mean",
  min_barcodes = 3,
  empirical_fit = "central"
)
```

## Arguments

- table:

  A table with one row for each barcode and the columns Gene, Log2FC and
  score.

- score:

  The column with the barcode scores.

- direction:

  "Down" or "Up".

- statistic:

  "mean" or "median".

- min_barcodes:

  Genes with fewer barcodes are not tested.

- empirical_fit:

  "central" or "one-sided".

## Value

A tibble with the columns Gene, NBarcodes, Score, Log2FC, Statistic and
PValue, with the attribute "null" (mean and sd of the fitted null).

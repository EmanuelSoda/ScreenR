# Scores of the Reference Genes

Returns the scores of the essential and non-essential genes, averaged by
gene or barcode by barcode.

## Usage

``` r
reference_scores(table, essential, nonessential, score, level)
```

## Arguments

- table:

  A table with one row for each barcode and the columns Gene and score,
  for example the output of
  [`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
  or
  [`compute_barcode_statistics`](https://emanuelsoda.github.io/ScreenR/reference/compute_barcode_statistics.md).

- essential:

  The names of the essential genes (as in the Gene column).

- nonessential:

  The names of the non-essential genes.

- score:

  The column with the score (default "Log2FC").

- level:

  "gene" (default) to average the scores of the barcodes of each gene,
  or "barcode" to use each barcode.

## Value

A list with the numeric vectors essential and nonessential.

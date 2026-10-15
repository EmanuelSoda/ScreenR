# Scale scores to reference genes

Rescales the scores of a screen so that the median of the non-essential
genes is 0 and the median of the essential genes is -1, as done for
example by BAGEL2 and Chronos. Scaled scores can be compared between
screens with different strength (for example different cell lines or
time points): a scaled log2 fold change of -1 means "as depleted as a
typical essential gene".

## Usage

``` r
scale_by_reference(table, essential, nonessential, score = "Log2FC")
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

## Value

The table with a new column named "Scaled" followed by the name of the
score column (for example ScaledLog2FC).

## Details

With \\m_N\\ and \\m_E\\ the medians of the scores of the non-essential
and essential genes (gene-level means of the barcode scores), the scaled
score of each barcode is \\(x - m_N) / (m_N - m_E)\\.

## See also

[`compute_screen_quality`](https://emanuelsoda.github.io/ScreenR/reference/compute_screen_quality.md),
[`hart_reference_genes`](https://emanuelsoda.github.io/ScreenR/reference/hart_reference_genes.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
metrics <- compute_metrics(object,
    control = "TRT", treatment = "Time3", day = "Time3"
)
# Example only: two arbitrary sets of genes used as references
genes <- unique(as.character(metrics$Gene))
scaled <- scale_by_reference(metrics,
    essential = genes[1:20], nonessential = genes[21:60]
)
#> Warning: The essential genes are not more depleted than the non-essential genes: the scaled scores are not meaningful.
head(scaled$ScaledLog2FC)
#> [1] -16.244149   6.332980  94.471041   3.249938 -10.591691  21.876512
```

# Compute the quality of a screen from reference genes

Measures how well a screen separates genes known to be essential from
genes known to be non-essential, the standard quality check of
proliferation (dropout) screens, both CRISPR and shRNA. In a good screen
the essential genes are clearly more depleted than the non-essential
ones. The reference genes can be the core essential and non-essential
genes of Hart et al.
([`hart_reference_genes`](https://emanuelsoda.github.io/ScreenR/reference/hart_reference_genes.md))
or any other lists.

## Usage

``` r
compute_screen_quality(
  table,
  essential,
  nonessential,
  score = "Log2FC",
  level = c("gene", "barcode")
)
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

A one-row tibble with the columns NEssential and NNonessential (the
number of genes or barcodes used), MeanEssential, MeanNonessential,
NNMD, CohenD and AUC.

## Details

The scores (by default the log2 fold changes) are averaged over the
barcodes of each gene (`level = "gene"`) or used barcode by barcode
(`level = "barcode"`). With \\x_E\\ the scores of the essential genes
and \\x_N\\ those of the non-essential genes:

- NNMD, the null-normalized mean difference (Hart et al. 2015):
  \\(mean(x_E) - mean(x_N)) / sd(x_N)\\. More negative is better.

- Cohen's d: the difference of the means divided by the pooled standard
  deviation. More negative is better.

- ROC AUC: the probability that an essential gene has a lower score than
  a non-essential gene (0.5: no separation, 1: perfect separation).

The reference genes not present in the table are ignored; the number of
genes used is reported.

## References

Hart T, Chandrashekhar M, Aregger M, et al. (2015). High-resolution
CRISPR screens reveal fitness genes and genotype-specific cancer
liabilities. Cell 163:1515-1526.
[doi:10.1016/j.cell.2015.11.015](https://doi.org/10.1016/j.cell.2015.11.015)

## See also

[`scale_by_reference`](https://emanuelsoda.github.io/ScreenR/reference/scale_by_reference.md),
[`plot_reference_separation`](https://emanuelsoda.github.io/ScreenR/reference/plot_reference_separation.md),
[`hart_reference_genes`](https://emanuelsoda.github.io/ScreenR/reference/hart_reference_genes.md)

## Examples

``` r
data(k562_count_table)
data(k562_annotation_table)
data(hart_reference_genes)
samples <- data.frame(
    Sample = c("Plasmid", "T14_A", "T14_B"), Day = "Screen",
    Treatment = c("Plasmid", "T14", "T14")
)
object <- create_screenr_object(
    table = k562_count_table, annotation = k562_annotation_table,
    groups = factor(c("Plasmid", "T14", "T14")), sample_table = samples
)
object <- compute_data_table(normalize_data(object))
metrics <- compute_metrics(object,
    control = "Plasmid", treatment = "T14", day = "Screen"
)
essential <- hart_reference_genes$Gene[
    hart_reference_genes$Class == "core essential"]
nonessential <- hart_reference_genes$Gene[
    hart_reference_genes$Class == "non-essential"]
compute_screen_quality(metrics, essential, nonessential)
#> # A tibble: 1 × 7
#>   NEssential NNonessential MeanEssential MeanNonessential  NNMD CohenD   AUC
#>        <int>         <int>         <dbl>            <dbl> <dbl>  <dbl> <dbl>
#> 1        660           890         -1.07          -0.0380 -2.39  -1.49 0.838
```

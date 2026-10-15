# Find hits with a gene-level Z-score test

Gene-level test for the Z-score method, with a p-value and a False
Discovery Rate (FDR) for each gene. Each gene gets a score, the mean (or
the median) of the Z-scores of its barcodes, which is compared with the
scores of random sets of barcodes of the same size. Use it on the table
returned by
[`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
or
[`compute_barcode_statistics`](https://emanuelsoda.github.io/ScreenR/reference/compute_barcode_statistics.md);
it replaces
[`find_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md).

## Usage

``` r
find_gene_zscore_hit(
  table_treate_vs_control,
  score = c("Zscore", "ZscoreRobust"),
  direction = c("Down", "Up"),
  statistic = c("mean", "median"),
  control_genes = NULL,
  n_permutations = 1e+05,
  min_barcodes = 3,
  fdr = 0.05,
  fdr_method = c("BH", "storey"),
  seed = NULL
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

  "Down" (default) for depleted genes or "Up" for enriched genes.

- statistic:

  The gene score: "mean" (default) or "median" of the barcode scores.
  The median is less affected by single barcodes with a strong effect
  (for example off-target effects) but has less power.

- control_genes:

  Optional names of control genes (as in the Gene column), for example
  "non-targeting". When given, the random barcodes of the permutations
  are drawn from their barcodes, and they are not tested. Use only
  controls that behave like genes without effect (see Details); the
  default (NULL) uses all the barcodes.

- n_permutations:

  The number of random barcode sets used to compute the p-values
  (default 1e5, at least 100).

- min_barcodes:

  Genes with fewer barcodes than this are not tested (default 3).

- fdr:

  The FDR threshold: only genes with an FDR lower than or equal to this
  value are returned (default 0.05). Use fdr = 1 to return all the
  tested genes.

- fdr_method:

  How the FDR is computed: "BH" (Benjamini-Hochberg, the default) or
  "storey" (Benjamini-Hochberg multiplied by the estimated proportion of
  genes without effect, less conservative when many genes have an
  effect).

- seed:

  An optional random seed, to make the permutations reproducible. The
  random number state of the session is restored afterwards.

## Value

A tibble with one row for each hit and the columns Gene, NBarcodes
(number of barcodes), Score (mean or median score of the barcodes),
Log2FC (median log2 fold change of the barcodes), PValue and FDR,
ordered by p-value.

## Details

Under the null hypothesis that the gene has no effect, its barcodes
behave like barcodes taken at random, so the p-value is computed by
permutation: the gene score is compared with the scores of
n_permutations random sets of barcodes of the same size. The p-values
are continuous (the smallest is 1 / (n_permutations + 1)) and do not
depend on the distribution of the barcode scores.

The random barcodes are drawn from all the barcodes or, when
control_genes is given, only from the barcodes of the control genes (for
example the non-targeting shRNAs). With all the barcodes (the default),
the barcodes of the true hits are part of the reference, which makes the
test conservative when many genes have an effect. Control genes avoid
this, but only if their barcodes behave like the barcodes of genes
without effect. This is often not true for non-targeting shRNAs:
targeting shRNAs are usually somewhat depleted compared with them
(off-target effects, toxicity of the RNAi machinery), so every gene
looks depleted. In the K562 essentiality screen
([`k562_count_table`](https://emanuelsoda.github.io/ScreenR/reference/k562_count_table.md))
the non-targeting reference called hundreds of non-essential genes,
while the default reference did not. Before using control genes, check
that their scores match those of genes known to have no effect (for
example the non-essential genes); otherwise use the default.

This replaces the rule of
[`find_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md)
("more than number_barcode barcodes below the median"), which flags
about 17% of the genes without any effect when genes have 10 barcodes
and number_barcode = 6.

## See also

[`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md),
[`compute_barcode_statistics`](https://emanuelsoda.github.io/ScreenR/reference/compute_barcode_statistics.md),
[`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
[`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
[`summarise_hits`](https://emanuelsoda.github.io/ScreenR/reference/summarise_hits.md),
[`find_discordant_barcodes`](https://emanuelsoda.github.io/ScreenR/reference/find_discordant_barcodes.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
table <- compute_metrics(object,
    control = "Time3", treatment = "TRT",
    day = "Time3"
)
find_gene_zscore_hit(table, fdr = 1, n_permutations = 1000, seed = 1)
#> # A tibble: 534 × 6
#>    Gene     NBarcodes  Score  Log2FC  PValue   FDR
#>    <chr>        <int>  <dbl>   <dbl>   <dbl> <dbl>
#>  1 Gene_377        10 -1.11  -0.259  0.00200 0.800
#>  2 Gene_500         9 -1.17  -0.152  0.00300 0.800
#>  3 Gene_86         10 -0.855 -0.340  0.00500 0.839
#>  4 Gene_511         9 -0.906 -0.203  0.00899 0.839
#>  5 Gene_173        10 -0.724 -0.221  0.0120  0.839
#>  6 Gene_316        10 -0.707 -0.117  0.0130  0.839
#>  7 Gene_438         9 -0.675 -0.0995 0.0220  0.839
#>  8 Gene_116        10 -0.636 -0.275  0.0250  0.839
#>  9 Gene_340        10 -0.637 -0.115  0.0250  0.839
#> 10 Gene_353        10 -0.636 -0.176  0.0250  0.839
#> # ℹ 524 more rows
```

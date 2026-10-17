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
  seed = NULL,
  reference = c("random", "empirical"),
  empirical_fit = c("central", "one-sided")
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
  (default 1e5, at least 100). Not used with reference = "empirical".

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

- reference:

  How the null is obtained: "random" (default), random sets of barcodes
  (from all the barcodes, or from those of control_genes), or
  "empirical", an empirical null of the gene statistics estimated from
  the data (see Details).

- empirical_fit:

  With reference = "empirical", how the null is fitted: "central"
  (default) or "one-sided" (see Details).

## Value

A tibble with one row for each hit and the columns Gene, NBarcodes
(number of barcodes), Score (mean or median score of the barcodes),
Log2FC (median log2 fold change of the barcodes), PValue and FDR,
ordered by p-value. With reference = "empirical" it also has the column
Statistic (the standardized gene score) and the attribute "null", the
mean and standard deviation of the fitted null.

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

**Empirical null (reference = "empirical").** Instead of random
barcodes, the gene scores are compared with the distribution of the
scores of the genes without effect, estimated from the data (an
empirical null, as in Efron's locfdr). Each gene gets a statistic, its
score standardized by the median and the median absolute deviation of
all the barcode scores and by the square root of its number of barcodes.
Most genes have no effect, so the centre of the distribution of these
statistics is the null: a normal distribution is fitted there
(empirical_fit = "central": a parabola fitted to the log density around
its mode; "one-sided": the mode and the spread of the statistics on the
side without hits, above the mode for "Down"), and the p-values come
from it. Two things make it better than the random barcodes in a dropout
screen: the hits do not widen the null, and the null includes the
variation between genes. The barcodes of a gene often vary together (the
same knockdown efficiency, the same position in the genome), so the
scores of genes without effect vary more than random sets of barcodes;
the random barcodes ignore this and give too small p-values. Use
[`plot_empirical_null`](https://emanuelsoda.github.io/ScreenR/reference/plot_empirical_null.md)
to check the fit.

In the benchmarks (core essential and non-essential genes of Hart et
al., half of the non-essential genes kept out to count the false calls),
in the genome-wide HAP1 CRISPR screen (TKOv3) the default found 58% of
the core essential genes at 5% FDR and the empirical null 90%
("central") and 94% ("one-sided"), with 2 and 5 non-essential genes
called. In the K562 shRNA screen the random barcodes gave too small
p-values: with control_genes 20% of the non-essential genes had a
p-value below 0.05 (50 to 57 called), and with the default 13% when the
essential genes were as rare as in a genome-wide library. The empirical
null kept this below 4% (no non-essential gene called) and found 30% of
the essential genes: the shRNAs of many essential genes do not separate
them from the genes without effect. "central" never gave too small
p-values; "one-sided" has more power when the hits are well separated
(CRISPR screens) but was too conservative in the shRNA screen.

The empirical null needs most genes to be without effect, and enough
genes (hundreds) to estimate it. It is not used with control_genes. The
benchmark script is benchmark/realdata/empirical_null.R in the GitHub
repository. The vignette "Choosing the reference of the gene-level test"
([`vignette("Empirical_null", package = "ScreenR")`](https://emanuelsoda.github.io/ScreenR/articles/Empirical_null.md))
explains when to use each reference; we suggest reference = "empirical"
for libraries with hundreds of genes, most of them without effect.

This replaces the rule of
[`find_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md)
("more than number_barcode barcodes below the median"), which flags
about 17% of the genes without any effect when genes have 10 barcodes
and number_barcode = 6.

## See also

[`plot_empirical_null`](https://emanuelsoda.github.io/ScreenR/reference/plot_empirical_null.md),
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
#>  1 Gene_377        10 -1.11  -0.259  0.00200 0.934
#>  2 Gene_500         9 -1.17  -0.152  0.00400 0.934
#>  3 Gene_86         10 -0.855 -0.340  0.00699 0.934
#>  4 Gene_511         9 -0.906 -0.203  0.0140  0.934
#>  5 Gene_245        32 -0.402 -0.0162 0.0160  0.934
#>  6 Gene_173        10 -0.724 -0.221  0.0200  0.934
#>  7 Gene_316        10 -0.707 -0.117  0.0220  0.934
#>  8 Gene_116        10 -0.636 -0.275  0.0270  0.934
#>  9 Gene_305        10 -0.634 -0.271  0.0270  0.934
#> 10 Gene_340        10 -0.637 -0.115  0.0270  0.934
#> # ℹ 524 more rows

# Empirical null
find_gene_zscore_hit(table, fdr = 1, reference = "empirical")
#> # A tibble: 534 × 7
#>    Gene     NBarcodes  Score  Log2FC Statistic    PValue    FDR
#>    <chr>        <int>  <dbl>   <dbl>     <dbl>     <dbl>  <dbl>
#>  1 Gene_377        10 -1.11  -0.259      -4.86 0.0000963 0.0261
#>  2 Gene_500         9 -1.17  -0.152      -4.86 0.0000978 0.0261
#>  3 Gene_511         9 -0.906 -0.203      -3.77 0.00195   0.274 
#>  4 Gene_86         10 -0.855 -0.340      -3.75 0.00205   0.274 
#>  5 Gene_173        10 -0.724 -0.221      -3.18 0.00749   0.668 
#>  6 Gene_245        32 -0.402 -0.0162     -3.17 0.00767   0.668 
#>  7 Gene_316        10 -0.707 -0.117      -3.10 0.00875   0.668 
#>  8 Gene_438         9 -0.675 -0.0995     -2.81 0.0157    0.725 
#>  9 Gene_340        10 -0.637 -0.115      -2.80 0.0162    0.725 
#> 10 Gene_353        10 -0.636 -0.176      -2.79 0.0163    0.725 
#> # ℹ 524 more rows
```

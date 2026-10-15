# Find hits with ROAST or FRY

Finds the genes whose barcodes are depleted (or enriched) in the
comparison given by the contrast, using the
[ROAST](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2922896/) method
([`mroast`](https://rdrr.io/pkg/limma/man/roast.html)) or its fast
approximation FRY ([`fry`](https://rdrr.io/pkg/limma/man/roast.html)).
ROAST is a self-contained gene set test: it tests whether the barcodes
of a gene change, without comparing them with the other barcodes, so it
is not affected by the number of genes with an effect. It needs
replicates in both groups (see Details).

## Usage

``` r
find_roast_hit(
  screenR_Object,
  matrix_model,
  contrast,
  nrot = 9999,
  number_barcode = 3,
  direction = c("Down", "Up"),
  p_val = NULL,
  fdr = 0.05,
  seed = NULL,
  method = c("roast", "fry"),
  fdr_method = c("BH", "storey")
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- matrix_model:

  The design matrix of the linear model, for example created with
  [`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md)
  (or with [`model.matrix`](https://rdrr.io/r/stats/model.matrix.html)).

- contrast:

  The comparison to test: the name or index of a column of matrix_model,
  a numeric contrast vector (for example created with
  [`create_contrast`](https://emanuelsoda.github.io/ScreenR/reference/create_contrast.md))
  or a one-column matrix created with
  [`makeContrasts`](https://rdrr.io/pkg/limma/man/makeContrasts.html).

- nrot:

  The number of random rotations used by method = "roast" (default
  9999). More rotations give more precise p-values and allow smaller
  ones (the smallest is 1 / (nrot + 1)), but take longer. Not used by
  method = "fry".

- number_barcode:

  Only genes with more than number_barcode barcodes are tested (default
  3, so a gene needs at least 4 barcodes). The barcodes of a gene are
  tested together as a set, so this is not a number of barcodes that
  must change.

- direction:

  The direction of the change: "Down" (default) for depleted genes or
  "Up" for enriched genes.

- p_val:

  An optional cut off on the raw (not adjusted) one-sided p-value: only
  genes with PValue \< p_val are returned. By default (NULL) only the
  FDR cut off is used.

- fdr:

  The FDR threshold: only genes with an FDR lower than or equal to this
  value are returned (default 0.05). Use fdr = 1 to return all the genes
  with the given direction.

- seed:

  An optional random seed. ROAST uses random rotations, so the p-values
  change slightly between runs; setting a seed makes the result
  reproducible. The random number state of the session is restored
  afterwards. Not used by method = "fry".

- method:

  "roast" (default, random rotations) or "fry" (fast exact
  approximation, recommended for large screens).

- fdr_method:

  How the FDR is computed: "BH" (Benjamini-Hochberg, the default) or
  "storey" (Benjamini-Hochberg multiplied by the estimated proportion of
  genes without effect, less conservative when many genes have an
  effect).

## Value

A tibble with one row for each hit in the requested direction and the
columns returned by [`mroast`](https://rdrr.io/pkg/limma/man/roast.html)
(Gene, NGenes, PropDown, PropUp, Direction, PValue, FDR, PValue.Mixed,
FDR.Mixed) or by [`fry`](https://rdrr.io/pkg/limma/man/roast.html)
(without PropDown and PropUp), plus Log2FC, the median log2 fold change
of the barcodes of the gene. NGenes is the number of barcodes of the
gene. PValue is the one-sided p-value for the requested direction and
FDR is computed from it over all the genes; PValue.Mixed and FDR.Mixed
are those of limma (a change in either direction).

## Details

The raw counts are modelled with edgeR (the dispersion is estimated with
[`estimateDisp`](https://rdrr.io/pkg/edgeR/man/estimateDisp.html)) and
the barcodes of each gene are tested together. The p-values are computed
with random rotations (method = "roast", whose smallest possible p-value
is 1 / (nrot + 1)) or with
[`fry`](https://rdrr.io/pkg/limma/man/roast.html) (method = "fry"), a
fast and exact approximation of roast with an unlimited number of
rotations, which is faster and gives continuous p-values. The two-sided
p-values are converted into one-sided p-values for the requested
direction and the FDR is computed from them over all the tested genes.

Since roast tests each gene on its own barcodes, its power depends on
the variability between replicates: it needs replicates in both the
treated and the control group and has little power with two replicates
or noisy data. With a single control sample (for example a plasmid
library) it finds almost no hits; in this case use
[`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)
or
[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
which compare each gene with the other genes. Being self-contained,
roast also reports genes whose barcodes change because of off-target
effects.

## See also

[`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
[`find_time_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md),
[`summarise_hits`](https://emanuelsoda.github.io/ScreenR/reference/summarise_hits.md),
[`plot_barcode_hit`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md),
[`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md),
[`create_contrast`](https://emanuelsoda.github.io/ScreenR/reference/create_contrast.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
matrix_model <- create_design_matrix(object)
contrast <- create_contrast(matrix_model, treatment = "Treated",
    control = "Control")

result <- find_roast_hit(object,
    matrix_model = matrix_model,
    contrast = contrast, nrot = 100, seed = 42
)
#> Warning: 3rows with all zero counts
head(result)
#> # A tibble: 0 × 10
#> # ℹ 10 variables: Gene <chr>, NGenes <int>, PropDown <dbl>, PropUp <dbl>,
#> #   Direction <fct>, PValue <dbl>, FDR <dbl>, PValue.Mixed <dbl>,
#> #   FDR.Mixed <dbl>, Log2FC <dbl>

# FRY: faster, with continuous p-values (fdr = 1 returns all the genes)
result <- find_roast_hit(object,
    matrix_model = matrix_model,
    contrast = contrast, method = "fry", fdr = 1
)
#> Warning: 3rows with all zero counts
head(result)
#> # A tibble: 6 × 8
#>   Gene     NGenes Direction   PValue   FDR PValue.Mixed FDR.Mixed  Log2FC
#>   <chr>     <int> <fct>        <dbl> <dbl>        <dbl>     <dbl>   <dbl>
#> 1 Gene_173     10 Down      0.000236 0.126      0.00330     0.588 -0.429 
#> 2 Gene_15      10 Down      0.00158  0.396      0.0226      0.998 -0.272 
#> 3 Gene_457     10 Down      0.00228  0.396      0.0446      0.998 -0.0230
#> 4 Gene_446     10 Down      0.00341  0.396      0.562       0.998 -0.131 
#> 5 Gene_121     10 Down      0.00379  0.396      0.00103     0.549 -0.194 
#> 6 Gene_293     10 Down      0.00508  0.396      0.00261     0.588 -0.108 
```

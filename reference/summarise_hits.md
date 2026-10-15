# Summarise the hits of several methods

Combines the results of several hit-finding methods in one table. For
each gene it reports the FDR of each method and the largest of them
(MaxFDR), a conservative score to rank the genes supported by all the
methods. To get a complete picture, compute the tables with fdr = 1 so
that they contain all the tested genes.

## Usage

``` r
summarise_hits(hit_list, fdr = 0.05)
```

## Arguments

- hit_list:

  A named list of tables, each with the columns Gene and FDR (for
  example from
  [`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
  [`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
  [`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md)
  or
  [`find_time_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md)).
  The names are used in the column names of the result.

- fdr:

  The FDR threshold used for the columns NMethods and Hit (default
  0.05).

## Value

A tibble with one row for each gene and the columns Gene,
FDR\_\<method\> for each method, NMethods (number of methods in which
the gene has FDR \<= fdr), MaxFDR and Hit (MaxFDR \<= fdr), ordered by
MaxFDR.

## Details

Since an FDR (Benjamini-Hochberg adjusted p-value) is never smaller than
the p-value, MaxFDR \<= alpha implies that the gene has a p-value \<=
alpha in every method: for each single gene this is a level-alpha
intersection-union test of the hypothesis "the gene has no effect in at
least one method", valid whatever the dependence between the methods.
Note however that the list of genes with MaxFDR \<= alpha does not have
a guaranteed False Discovery Rate: the false discoveries of the single
methods can concentrate in their intersection.

A gene missing from a table gets an NA FDR for that method and an NA
MaxFDR.

## See also

[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
[`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
[`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
[`find_time_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md),
[`find_common_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_common_hit.md)

## Examples

``` r
zscore <- data.frame(Gene = c("A", "B", "C"), FDR = c(0.01, 0.2, 0.03))
camera <- data.frame(Gene = c("A", "B", "C"), FDR = c(0.02, 0.01, 0.5))
summarise_hits(list(zscore = zscore, camera = camera))
#> # A tibble: 3 × 6
#>   Gene  FDR_zscore FDR_camera NMethods MaxFDR Hit  
#>   <chr>      <dbl>      <dbl>    <dbl>  <dbl> <lgl>
#> 1 A           0.01       0.02        2   0.02 TRUE 
#> 2 B           0.2        0.01        1   0.2  FALSE
#> 3 C           0.03       0.5         1   0.5  FALSE
```

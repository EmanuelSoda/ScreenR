# Combine evidence for an effect in every contrast

Tests whether each gene has an effect in a prespecified direction in all
supplied contrasts, using an intersection-union test.

## Usage

``` r
combine_consistent_hits(
  results,
  direction = c("Down", "Up"),
  fdr = 0.05,
  fdr_method = c("BH", "BY")
)
```

## Arguments

- results:

  A named list of at least two complete gene-level tables. Every table
  must contain the same unique, non-missing Gene identifiers and numeric
  PValue values between zero and one. PValue must be an unadjusted
  one-sided p-value for direction, including genes whose observed effect
  is opposite. Do not supply filtered hit lists or two-sided p-values.
  Optional Log2FC columns are retained.

- direction:

  The common, prespecified direction, "Down" or "Up". This labels the
  hypothesis; it does not convert input p-values. The caller must ensure
  that all tables test this direction.

- fdr:

  Maximum combined FDR to return (default 0.05); use 1 for all tested
  genes.

- fdr_method:

  Multiple-testing correction across genes: "BH" (default, independence
  or suitable positive dependence) or "BY" (arbitrary dependence).

## Value

A tibble ordered by combined PValue, with Gene, Direction (the tested
direction), NContrasts, PValue, FDR, PValue\_\<contrast\> and, when
supplied, Log2FC\_\<contrast\> columns. No pooled effect is computed.

## Details

For each gene the null is that at least one contrast has no effect in
the requested direction. The alternative is an effect in that direction
in every contrast. The combined p-value is the maximum of the individual
raw p-values. This combination allows arbitrary dependence between
contrasts, provided the individual p-values are valid. It does not
repair a misspecified model, an inappropriate reference distribution, or
ignored repeated measurements.

FDR adjustment is applied to the combined p-values over all supplied
genes, before filtering. Input FDR columns are ignored. Missing genes,
missing p-values and duplicated identifiers are errors: a missing test
cannot be silently omitted from an all-contrasts hypothesis. The
function cannot detect tables filtered to the same subset of genes;
supplying the complete testing universe is the caller's responsibility.

Consistency here means an effect in every specified contrast, not equal
effect sizes, a time interaction, or independent replication.

## References

Heller R, Golland Y, Malach R, Benjamini Y (2007). Conjunction group
analysis: an alternative to mixed/random effect analysis. NeuroImage
37:1178-1185. <https://pubmed.ncbi.nlm.nih.gov/17689266/>

## See also

[`find_consistent_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_consistent_hit.md),
[`find_time_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md)

## Examples

``` r
t3 <- data.frame(Gene = c("A", "B"), PValue = c(0.001, 0.01))
t4 <- data.frame(Gene = c("B", "A"), PValue = c(0.8, 0.002))
combine_consistent_hits(list(T3 = t3, T4 = t4))
#> # A tibble: 1 × 7
#>   Gene  Direction NContrasts PValue   FDR PValue_T3 PValue_T4
#>   <chr> <chr>          <int>  <dbl> <dbl>     <dbl>     <dbl>
#> 1 A     Down               2  0.002 0.004     0.001     0.002
```

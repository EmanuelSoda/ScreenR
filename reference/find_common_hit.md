# Find the hits in common between methods

Returns the genes found as hits by at least common_in of the given
methods. The results can come from any hit-finding method of ScreenR,
and from any number of methods. To combine the methods using their FDR,
see
[`summarise_hits`](https://emanuelsoda.github.io/ScreenR/reference/summarise_hits.md);
to plot the overlap, see
[`plot_common_hit`](https://emanuelsoda.github.io/ScreenR/reference/plot_common_hit.md).

## Usage

``` r
find_common_hit(..., common_in = NULL, fdr = NULL)
```

## Arguments

- ...:

  The results of the methods: gene-level tables with a Gene column (for
  example from
  [`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
  [`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
  [`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
  [`find_time_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md)
  or
  [`find_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md))
  or vectors of gene names, optionally named; or a single named list of
  them.

- common_in:

  The minimum number of methods in which a gene has to be found. By
  default (NULL) all the methods.

- fdr:

  An optional FDR threshold: the genes of the results with an FDR column
  are kept only when their FDR is at most fdr (useful for results
  computed with fdr = 1). By default (NULL) all the genes of each result
  are used.

## Value

A character vector with the names of the common hits, ordered by the
number of methods that find them.

## See also

[`plot_common_hit`](https://emanuelsoda.github.io/ScreenR/reference/plot_common_hit.md),
[`summarise_hits`](https://emanuelsoda.github.io/ScreenR/reference/summarise_hits.md)

## Examples

``` r
hit_zscore <- data.frame(Gene = c("A", "B", "C", "D", "E"))
hit_camera <- data.frame(Gene = c("A", "B", "C", "F", "H", "G"))
hit_roast <- data.frame(Gene = c("A", "L", "N"))

# Common to all the three methods
find_common_hit(hit_zscore, hit_camera, hit_roast)
#> [1] "A"

# Common to at least two of the three methods
find_common_hit(hit_zscore, hit_camera, hit_roast, common_in = 2)
#> [1] "A" "B" "C"

# Any number of methods, filtered on their FDR
results <- list(
    FRY = data.frame(Gene = c("A", "B", "C"), FDR = c(0.01, 0.02, 0.3)),
    CAMERA = data.frame(Gene = c("A", "C"), FDR = c(0.04, 0.01)),
    Zscore = c("A", "B"),
    ROAST = data.frame(Gene = c("A", "B"), FDR = c(0.01, 0.6))
)
find_common_hit(results, common_in = 3, fdr = 0.05)
#> [1] "A"
```

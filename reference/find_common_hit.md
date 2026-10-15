# Find the hits in common between methods

Returns the genes found as hits by at least common_in of the three
methods (Z-score, CAMERA and ROAST). Only the gene names are used, so
the tables can come from any method. To combine the methods using their
FDR, see
[`summarise_hits`](https://emanuelsoda.github.io/ScreenR/reference/summarise_hits.md).

## Usage

``` r
find_common_hit(hit_zscore, hit_camera, hit_roast, common_in = 3)
```

## Arguments

- hit_zscore:

  The hits of the Z-score method, a table with a Gene column (for
  example from
  [`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md)
  or
  [`find_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md)).

- hit_camera:

  The hits of
  [`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
  a table with a Gene column.

- hit_roast:

  The hits of
  [`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
  a table with a Gene column.

- common_in:

  The minimum number of methods in which a gene has to be found to be
  returned. The default, 3, returns the genes found by all three
  methods.

## Value

A character vector with the names of the common hits.

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
```

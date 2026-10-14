# Find common hit

This method find the hit in common between the three methods

## Usage

``` r
find_common_hit(hit_zscore, hit_camera, hit_roast, common_in = 3)
```

## Arguments

- hit_zscore:

  The matrix obtained by the
  [`find_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md)
  method

- hit_camera:

  The matrix obtained by the
  [`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)
  method

- hit_roast:

  The matrix obtained by the
  [`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md)
  method

- common_in:

  Number of methods in which the hit has to be in common in order to be
  considered a candidate hit. The default value is 3, which means that
  has to be present in the result of all the three methods.

## Value

A vector containing the common hit

## Examples

``` r
hit_zscore <- data.frame(Gene = c("A", "B", "C", "D", "E"))
hit_camera <- data.frame(Gene = c("A", "B", "C", "F", "H", "G"))
hit_roast <- data.frame(Gene = c("A", "L", "N"))

# common among all the three methods
find_common_hit(hit_zscore, hit_camera, hit_roast)
#> [1] "A"

# common among at least two of the three methods
find_common_hit(hit_zscore, hit_camera, hit_roast, common_in = 2)
#> [1] "A" "B" "C"
```

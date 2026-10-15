# Create the design matrix

Creates the design matrix used by the edgeR/limma based methods
([`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
[`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
[`compute_barcode_statistics`](https://emanuelsoda.github.io/ScreenR/reference/compute_barcode_statistics.md),
[`filter_by_variance`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_variance.md)
and
[`plot_barcode_hit`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md))
from the groups of the ScreenR object. The matrix has no intercept: it
has one column for each group, so that each coefficient is the mean of a
group and a comparison between groups is expressed with a contrast
created by
[`create_contrast`](https://emanuelsoda.github.io/ScreenR/reference/create_contrast.md).
The column names are syntactically valid versions of the group names
(for example "T1/T2" becomes "T1.T2").

## Usage

``` r
create_design_matrix(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

A design matrix with one row for each sample and one column for each
group.

## See also

[`create_contrast`](https://emanuelsoda.github.io/ScreenR/reference/create_contrast.md),
[`get_groups`](https://emanuelsoda.github.io/ScreenR/reference/get_groups.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
create_design_matrix(object)
#>    Control T1.T2 Treated
#> 1        0     1       0
#> 2        0     1       0
#> 3        0     0       1
#> 4        0     0       1
#> 5        0     0       1
#> 6        1     0       0
#> 7        1     0       0
#> 8        1     0       0
#> 9        0     0       1
#> 10       0     0       1
#> 11       0     0       1
#> 12       1     0       0
#> 13       1     0       0
#> 14       1     0       0
#> attr(,"assign")
#> [1] 1 1 1
#> attr(,"contrasts")
#> attr(,"contrasts")$groups
#> [1] "contr.treatment"
#> 
```

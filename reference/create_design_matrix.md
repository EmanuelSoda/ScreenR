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
(for example "T1/T2" becomes "T1.T2"). Row names are sample identifiers
in the order of the object. All model-based ScreenR functions check that
this order matches the object. Recreate the design after subsetting or
reordering samples. For a custom design from model.matrix(), explicitly
set its row names to the identifiers of the samples used to construct
its rows; unnamed rows or default numeric row names do not establish
sample alignment. Named contrast vectors are aligned to the design
columns; unnamed vectors are interpreted in column order.

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
#>             Control T1.T2 Treated
#> T1                0     1       0
#> T2                0     1       0
#> Time3_TRT_A       0     0       1
#> Time3_TRT_B       0     0       1
#> Time3_TRT_C       0     0       1
#> Time3_A           1     0       0
#> Time3_B           1     0       0
#> Time3_C           1     0       0
#> Time4_TRT_A       0     0       1
#> Time4_TRT_B       0     0       1
#> Time4_TRT_C       0     0       1
#> Time4_A           1     0       0
#> Time4_B           1     0       0
#> Time4_c           1     0       0
#> attr(,"assign")
#> [1] 1 1 1
#> attr(,"contrasts")
#> attr(,"contrasts")$groups
#> [1] "contr.treatment"
#> 
```

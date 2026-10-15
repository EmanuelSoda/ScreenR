# Update a ScreenR object

Converts a ScreenR object created with a version of ScreenR older than
1.15.8 to the current class, which extends
[`SummarizedExperiment`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html).
All the ScreenR functions do this automatically, so it is only needed to
use the object with other Bioconductor tools. Objects of the current
class are returned unchanged.

## Usage

``` r
update_screenr_object(object)
```

## Arguments

- object:

  A ScreenR object.

## Value

A ScreenR object of the current class.

## See also

[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Examples

``` r
# The example object of the package was saved with an older version
object <- get0("object", envir = asNamespace("ScreenR"))
object <- update_screenr_object(object)
object
#> class: screenr_object 
#> dim: 5320 14 
#> metadata(0):
#> assays(2): counts normalized
#> rownames(5320): 10208 10502 ... 13876 13995
#> rowData names(5): Barcode Gene Gene_ID Sequence Library
#> colnames(14): T1 T2 ... Time4_B Time4_c
#> colData names(4): Sample Day Treatment Group
#> data table: 74480 rows, 9 columns
```

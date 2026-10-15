# Show a ScreenR object

Prints a summary of a ScreenR object: the SummarizedExperiment summary
and the size of the data table. Objects created with older versions of
ScreenR are converted first (see
[`update_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/update_screenr_object.md)).

## Usage

``` r
# S4 method for class 'screenr_object'
show(object)
```

## Arguments

- object:

  A ScreenR object.

## Value

NULL, invisibly.

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
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

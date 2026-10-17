# Show a ScreenR object

Prints a summary of a ScreenR object: the SummarizedExperiment summary
and the normalization. Objects created with older versions of ScreenR
are converted first (see
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
#> metadata(2): screenr_layout normalization
#> assays(1): counts
#> rownames(5320): 10208 10502 ... 13876 13995
#> rowData names(5): Barcode Gene Gene_ID Sequence Library
#> colnames(14): T1 T2 ... Time4_B Time4_c
#> colData names(5): Sample Day Treatment Group EffectiveLibSize
#> normalization: cpm (effective library sizes in colData(object)$EffectiveLibSize)
```

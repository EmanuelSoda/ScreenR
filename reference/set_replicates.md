# Set the replicates

Replaces the replicate labels of a ScreenR object.

## Usage

``` r
set_replicates(object, replicates)

# S4 method for class 'screenr_object'
set_replicates(object, replicates)
```

## Arguments

- object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- replicates:

  A vector with the replicate labels.

## Value

The ScreenR object with the new replicates.

## See also

[`get_replicates`](https://emanuelsoda.github.io/ScreenR/reference/get_replicates.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
replicates <- get_replicates(object)
set_replicates(object, replicates)
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

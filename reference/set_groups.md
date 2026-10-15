# Set the groups

Replaces the experimental groups of the samples of a ScreenR object. A
warning is given when the number of groups differs from the number of
samples.

## Usage

``` r
set_groups(object, groups)

# S4 method for class 'screenr_object'
set_groups(object, groups)
```

## Arguments

- object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- groups:

  A factor (or a vector, which is converted to a factor) with the group
  of each sample, for example "Control" and "Treated".

## Value

The ScreenR object with the new groups.

## See also

[`get_groups`](https://emanuelsoda.github.io/ScreenR/reference/get_groups.md),
[`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
groups <- get_groups(object)
set_groups(object, groups)
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

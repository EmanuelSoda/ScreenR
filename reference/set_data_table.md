# Set the data table

Deprecated. The data table is no longer stored: it is computed from the
counts, the annotation, the sample table and the normalization of the
object when needed (see
[`get_data_table`](https://emanuelsoda.github.io/ScreenR/reference/get_data_table.md)),
so it cannot be replaced. Given the current data table, the object is
returned unchanged with a deprecation warning; any other table is an
error. To change the data table, change the object (subset it, or use
[`set_annotation_table`](https://emanuelsoda.github.io/ScreenR/reference/set_annotation_table.md)
or
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)).

## Usage

``` r
set_data_table(object, data_table)

# S4 method for class 'screenr_object'
set_data_table(object, data_table)
```

## Arguments

- object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- data_table:

  A data table in tidy (long) format, with the columns described in
  [`get_data_table`](https://emanuelsoda.github.io/ScreenR/reference/get_data_table.md).

## Value

The unchanged ScreenR object (with a warning).

## See also

[`compute_data_table`](https://emanuelsoda.github.io/ScreenR/reference/compute_data_table.md),
[`get_data_table`](https://emanuelsoda.github.io/ScreenR/reference/get_data_table.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
data_table <- get_data_table(object)
#> ScreenR normalized data table: 74480 rows, 9 columns
set_data_table(object, data_table)
#> Warning: set_data_table() is deprecated: the data table is computed from the object when needed.
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

# Set the normalized count table

Replaces the normalized count table of a ScreenR object, for example
with counts normalized outside ScreenR. The table is matched to the
count table by barcode and sample; an empty table removes the normalized
counts. Usually the normalized counts are computed with
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md).

## Usage

``` r
set_normalized_count_table(object, normalized_count_table)

# S4 method for class 'screenr_object'
set_normalized_count_table(object, normalized_count_table)
```

## Arguments

- object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- normalized_count_table:

  A table with a Barcode column and one column of normalized counts for
  each sample.

## Value

The ScreenR object with the new normalized count table.

## See also

[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md),
[`get_normalized_count_table`](https://emanuelsoda.github.io/ScreenR/reference/get_normalized_count_table.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
normalized_count_table <- get_normalized_count_table(object)
#> ScreenR normalized count table: 5320 rows, 15 columns
set_normalized_count_table(object, normalized_count_table)
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

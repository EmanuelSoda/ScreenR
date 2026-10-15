# Set the data table

Replaces the data table of a ScreenR object. Usually the data table is
computed with
[`compute_data_table`](https://emanuelsoda.github.io/ScreenR/reference/compute_data_table.md).

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

The ScreenR object with the new data table.

## See also

[`compute_data_table`](https://emanuelsoda.github.io/ScreenR/reference/compute_data_table.md),
[`get_data_table`](https://emanuelsoda.github.io/ScreenR/reference/get_data_table.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
data_table <- get_data_table(object)
#> ScreenR normalized data table: 74480 rows, 9 columns
set_data_table(object, data_table)
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

# Set the count table

Replaces the raw count table of a ScreenR object. The annotation, the
groups and the normalized counts are kept (the normalized counts are
matched to the new table by barcode and sample); the Day and Treatment
of the samples are kept when the samples are the same, otherwise they
are taken again from the sample names. Unlike
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md),
the table is not checked.

## Usage

``` r
set_count_table(object, count_table)

# S4 method for class 'screenr_object'
set_count_table(object, count_table)
```

## Arguments

- object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- count_table:

  A count table with a Barcode column and one column of counts for each
  sample.

## Value

The ScreenR object with the new count table.

## See also

[`get_count_table`](https://emanuelsoda.github.io/ScreenR/reference/get_count_table.md),
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
counts <- get_count_table(object)
#> ScreenR count table: 5320 rows, 15 columns
set_count_table(object, counts)
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

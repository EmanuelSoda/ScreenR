# Get the count table

Returns the raw count table of a ScreenR object, with one row for each
barcode and one column for each sample. A short summary of the size of
the table is printed.

The table is a view of the counts assay (`assay(object, "counts")`): the
Barcode column is made from the row names and is not a sample. Changing
the returned table does not change the object; use
[`set_count_table`](https://emanuelsoda.github.io/ScreenR/reference/set_count_table.md).

## Usage

``` r
get_count_table(object)

# S4 method for class 'screenr_object'
get_count_table(object)
```

## Arguments

- object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

A tibble with the Barcode column and one column of counts for each
sample.

## See also

[`set_count_table`](https://emanuelsoda.github.io/ScreenR/reference/set_count_table.md),
[`get_normalized_count_table`](https://emanuelsoda.github.io/ScreenR/reference/get_normalized_count_table.md),
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
count_table <- get_count_table(object)
#> ScreenR count table: 5320 rows, 15 columns
head(count_table)
#> # A tibble: 6 × 15
#>   Barcode    T1    T2 Time3_TRT_A Time3_TRT_B Time3_TRT_C Time3_A Time3_B
#>   <chr>   <int> <int>       <int>       <int>       <int>   <int>   <int>
#> 1 10208    3520  4283        2493        3998        3018    3393    3829
#> 2 10502     280   339         140         542         162     468     157
#> 3 10796    1507  1533        1120        1125        1286    1136    1095
#> 4 11089    3797  4151        2296        3857        3645    3677    3124
#> 5 11382     717   644         564         336         256     554     590
#> 6 11675    7330  7593        4927        6211        6504    7781    7183
#> # ℹ 7 more variables: Time3_C <int>, Time4_TRT_A <int>, Time4_TRT_B <int>,
#> #   Time4_TRT_C <int>, Time4_A <int>, Time4_B <int>, Time4_c <int>
```

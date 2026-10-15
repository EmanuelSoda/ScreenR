# Remove the barcodes with zero counts in all samples

Removes the barcodes (rows) with zero counts in all the samples from the
count table, the annotation table and, when present, the normalized
count table and the data table. Run it before
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md).

## Usage

``` r
remove_all_zero_row(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

The ScreenR object without the barcodes with zero counts in all samples.

## See also

[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md),
[`barcode_lost`](https://emanuelsoda.github.io/ScreenR/reference/barcode_lost.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
counts <- get_count_table(object)
#> ScreenR count table: 5320 rows, 15 columns
nrow(counts)
#> [1] 5320
object <- remove_all_zero_row(object)
counts <- get_count_table(object)
#> ScreenR count table: 5317 rows, 15 columns
nrow(counts)
#> [1] 5317
```

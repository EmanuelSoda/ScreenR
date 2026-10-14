# Remove rows that have zero count in all samples

This function removes the rows that have zero count in all samples. It
takes care of updating both count_table and annotation_table.
Very_Important: It has to be performed before the data normalization.

## Usage

``` r
remove_all_zero_row(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Value

The ScreenR object with the count_table and the annotation_table
filtered.

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
counts <- get_count_table(object)
#> ScreenR count table containing:
#>  5320 rows
#>  15 columns
nrow(counts)
#> [1] 5320
object <- remove_all_zero_row(object)
counts <- get_count_table(object)
#> ScreenR count table containing:
#>  5317 rows
#>  15 columns
nrow(counts)
#> [1] 5317
```

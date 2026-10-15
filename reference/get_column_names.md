# Get the original column names

Returns the names that the columns had in the tables given to
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).
Internally the barcode and gene columns are always called Barcode and
Gene, and the sample information Sample, Day and Treatment.

## Usage

``` r
get_column_names(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

A named character vector: the names are the internal column names and
the values the original ones.

## See also

[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md),
[`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
get_column_names(object)
#>   Barcode      Gene 
#> "Barcode"    "Gene" 
```

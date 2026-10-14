# Count number of barcode lost

This function counts the number of barcodes lost during the sequencing.
A barcode is lost if its associated shRNA has zero mapped read in a
sample.

## Usage

``` r
barcode_lost(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Value

Return a tibble containing the number of barcode lost for each sample

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

# In order to count the number of barcodes lost just the ScreenR object is
# needed
head(barcode_lost(object))
#> # A tibble: 6 × 2
#>   Sample      LostBarcode
#>   <fct>             <int>
#> 1 T1                    5
#> 2 T2                   21
#> 3 Time3_TRT_A          27
#> 4 Time3_TRT_B          28
#> 5 Time3_TRT_C          27
#> 6 Time3_A               9
```

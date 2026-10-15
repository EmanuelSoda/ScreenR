# Normalize the counts

Normalizes the counts to counts per million (CPM), so that samples
sequenced at different depths can be compared. Run it before
[`compute_data_table`](https://emanuelsoda.github.io/ScreenR/reference/compute_data_table.md).
Three methods are available:

- "cpm" (default): each count is divided by the total number of reads of
  its sample and multiplied by 1e6.

- "tmm": as "cpm", but the library sizes are corrected with the trimmed
  mean of M-values (TMM) normalization factors of edgeR
  ([`normLibSizes`](https://rdrr.io/pkg/edgeR/man/calcNormFactors.html)).
  This is more robust when strong hits deplete a large part of the
  library.

- "control": each count is divided by the number of reads of the control
  barcodes (for example non-targeting shRNAs) of its sample and
  multiplied by 1e6. This assumes that the control barcodes are not
  affected by the treatment.

The normalization only affects the normalized count table (and so the
data table, the Z-score and the plots). The edgeR based methods (CAMERA,
ROAST) always model the raw counts.

## Usage

``` r
normalize_data(
  screenR_Object,
  method = c("cpm", "tmm", "control"),
  control_barcodes = NULL
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- method:

  The normalization method: "cpm" (default), "tmm" or "control".

- control_barcodes:

  The barcodes to use for the "control" method (required by that method,
  ignored by the others).

## Value

The ScreenR object with the normalized count table (see
[`get_normalized_count_table`](https://emanuelsoda.github.io/ScreenR/reference/get_normalized_count_table.md)).

## See also

[`compute_data_table`](https://emanuelsoda.github.io/ScreenR/reference/compute_data_table.md),
[`get_normalized_count_table`](https://emanuelsoda.github.io/ScreenR/reference/get_normalized_count_table.md),
[`remove_all_zero_row`](https://emanuelsoda.github.io/ScreenR/reference/remove_all_zero_row.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
object <- normalize_data(object)

get_normalized_count_table(object)
#> ScreenR normalized count table: 5320 rows, 15 columns
#> # A tibble: 5,320 × 15
#>    Barcode    T1    T2 Time3_TRT_A Time3_TRT_B Time3_TRT_C Time3_A Time3_B
#>    <fct>   <dbl> <dbl>       <dbl>       <dbl>       <dbl>   <dbl>   <dbl>
#>  1 10208   330.  437.        365.        439.        311.    358.    425. 
#>  2 10502    26.2  34.6        20.5        59.5        16.7    49.3    17.4
#>  3 10796   141.  156.        164.        124.        132.    120.    121. 
#>  4 11089   356.  424.        336.        424.        375.    388.    346. 
#>  5 11382    67.2  65.7        82.6        36.9        26.3    58.4    65.4
#>  6 11675   687.  775.        722.        682.        669.    820.    797. 
#>  7 11969   455.  548.        623.        540.        558.    488.    438. 
#>  8 12263   144.  119.        112.        138.         95.0   158.    128. 
#>  9 12555   406.  413.        459.        518.        483.    408.    425. 
#> 10 12847    43.7  42.6        30.2        34.9        29.6    34.6    20.5
#> # ℹ 5,310 more rows
#> # ℹ 7 more variables: Time3_C <dbl>, Time4_TRT_A <dbl>, Time4_TRT_B <dbl>,
#> #   Time4_TRT_C <dbl>, Time4_A <dbl>, Time4_B <dbl>, Time4_c <dbl>

# TMM normalization
object <- normalize_data(object, method = "tmm")
```

# Get the normalization

Returns the normalization of a ScreenR object, as set by
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md):
the method, the effective library size of each sample and how it was
obtained. The same normalization is used by all the functions of the
analysis: the normalized counts, the data table, the plots and the
offsets of the edgeR and limma models.

## Usage

``` r
get_normalization(object)
```

## Arguments

- object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

NULL when the object is not normalized; otherwise a list with:

- method: "cpm", "tmm", "control" or "custom" (set with
  [`set_normalized_count_table`](https://emanuelsoda.github.io/ScreenR/reference/set_normalized_count_table.md));

- effective_library_size: the effective library size of each sample of
  the object (the normalized count of a barcode is its raw count divided
  by the effective library size, times 1e6);

- reference: the identifiers of the control barcodes (method "control"),
  otherwise NULL;

- and the provenance recorded when the normalization was computed:
  library_size (the total raw counts of each sample), norm_factors (the
  TMM factors for "tmm", otherwise 1), n_barcodes (the number of
  barcodes of the object) and screenr_version.

The provenance describes the object when it was normalized: after
subsetting, the effective library sizes of the retained samples are kept
unchanged.

## See also

[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
get_normalization(object)$method
#> [1] "cpm"
get_normalization(normalize_data(object, method = "tmm"))
#> $method
#> [1] "tmm"
#> 
#> $effective_library_size
#>          T1          T2 Time3_TRT_A Time3_TRT_B Time3_TRT_C     Time3_A 
#>    10853404     9827013     6840595     9229604     9737088     9647737 
#>     Time3_B     Time3_C Time4_TRT_A Time4_TRT_B Time4_TRT_C     Time4_A 
#>     9179789     9641856     8799318     9717098    12044656    10302865 
#>     Time4_B     Time4_c 
#>     9128760     9182589 
#> 
#> $reference
#> NULL
#> 
#> $library_size
#>          T1          T2 Time3_TRT_A Time3_TRT_B Time3_TRT_C     Time3_A 
#>    10671757     9799724     6824877     9107354     9717868     9485195 
#>     Time3_B     Time3_C Time4_TRT_A Time4_TRT_B Time4_TRT_C     Time4_A 
#>     9016812     9568624     8933915     9883209    12325124    10351136 
#>     Time4_B     Time4_c 
#>     9180192     9315365 
#> 
#> $norm_factors
#>          T1          T2 Time3_TRT_A Time3_TRT_B Time3_TRT_C     Time3_A 
#>   1.0170212   1.0027846   1.0023031   1.0134232   1.0019778   1.0171364 
#>     Time3_B     Time3_C Time4_TRT_A Time4_TRT_B Time4_TRT_C     Time4_A 
#>   1.0180748   1.0076534   0.9849342   0.9831926   0.9772442   0.9953367 
#>     Time4_B     Time4_c 
#>   0.9943974   0.9857466 
#> 
#> $n_barcodes
#> [1] 5320
#> 
#> $screenr_version
#> [1] "1.15.16"
#> 
```

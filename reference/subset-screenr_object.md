# Subset a ScreenR object

Subsets the barcodes (rows) and the samples (columns) of a ScreenR
object, like any
[`SummarizedExperiment`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html).
The annotation (rowData) and the sample information (colData) are subset
with the counts, so they stay aligned by identifier when the rows or the
columns are reordered.

## Usage

``` r
# S4 method for class 'screenr_object,ANY,ANY,ANY'
x[i, j, ..., drop = TRUE]
```

## Arguments

- x:

  A ScreenR object.

- i, j:

  The barcodes and samples to keep (indices, names or logical vectors).

- ...:

  Not used.

- drop:

  Not used.

## Value

The subset ScreenR object.

## Details

The normalization is kept: the retained samples keep their effective
library sizes, which were computed on all the barcodes when
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
was called. To normalize on the retained barcodes only, call
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
again after subsetting.

## See also

[`remove_all_zero_row`](https://emanuelsoda.github.io/ScreenR/reference/remove_all_zero_row.md),
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
object[1:100, ]
#> class: screenr_object 
#> dim: 100 14 
#> metadata(2): screenr_layout normalization
#> assays(1): counts
#> rownames(100): 10208 10502 ... 12359 12651
#> rowData names(5): Barcode Gene Gene_ID Sequence Library
#> colnames(14): T1 T2 ... Time4_B Time4_c
#> colData names(5): Sample Day Treatment Group EffectiveLibSize
#> normalization: cpm (effective library sizes in colData(object)$EffectiveLibSize)
object[, object$Treatment == "TRT"]
#> class: screenr_object 
#> dim: 5320 6 
#> metadata(2): screenr_layout normalization
#> assays(1): counts
#> rownames(5320): 10208 10502 ... 13876 13995
#> rowData names(5): Barcode Gene Gene_ID Sequence Library
#> colnames(6): Time3_TRT_A Time3_TRT_B ... Time4_TRT_B Time4_TRT_C
#> colData names(5): Sample Day Treatment Group EffectiveLibSize
#> normalization: cpm (effective library sizes in colData(object)$EffectiveLibSize)
```

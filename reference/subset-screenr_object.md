# Subset a ScreenR object

Subsets the barcodes (rows) and the samples (columns) of a ScreenR
object, like any
[`SummarizedExperiment`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html).
The normalized counts and the data table are subset to the same barcodes
and samples.

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

## See also

[`remove_all_zero_row`](https://emanuelsoda.github.io/ScreenR/reference/remove_all_zero_row.md)

## Examples

``` r
object <- update_screenr_object(
    get0("object", envir = asNamespace("ScreenR"))
)
object[1:100, ]
#> class: screenr_object 
#> dim: 100 14 
#> metadata(0):
#> assays(2): counts normalized
#> rownames(100): 10208 10502 ... 12359 12651
#> rowData names(5): Barcode Gene Gene_ID Sequence Library
#> colnames(14): T1 T2 ... Time4_B Time4_c
#> colData names(4): Sample Day Treatment Group
#> data table: 1400 rows, 9 columns
object[, object$Treatment == "TRT"]
#> class: screenr_object 
#> dim: 5320 6 
#> metadata(0):
#> assays(2): counts normalized
#> rownames(5320): 10208 10502 ... 13876 13995
#> rowData names(5): Barcode Gene Gene_ID Sequence Library
#> colnames(6): Time3_TRT_A Time3_TRT_B ... Time4_TRT_B Time4_TRT_C
#> colData names(4): Sample Day Treatment Group
#> data table: 31920 rows, 9 columns
```

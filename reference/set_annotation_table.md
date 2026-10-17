# Set the annotation table

Replaces the annotation table of a ScreenR object. The annotation of the
barcodes of the count table (the rowData of the object) is updated by
matching the Barcode column, so the new genes are used by all the
following analyses (data table, metrics, edgeR/limma methods). Barcodes
annotated more than once with different values are an error.

## Usage

``` r
set_annotation_table(object, annotation_table)

# S4 method for class 'screenr_object'
set_annotation_table(object, annotation_table)
```

## Arguments

- object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- annotation_table:

  A data frame with the annotation of each barcode, with at least the
  columns Barcode and Gene.

## Value

The ScreenR object with the new annotation table.

## See also

[`get_annotation_table`](https://emanuelsoda.github.io/ScreenR/reference/get_annotation_table.md),
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
annotation <- get_annotation_table(object)
#> ScreenR annotation table: 5320 rows, 5 columns
set_annotation_table(object, annotation)
#> class: screenr_object 
#> dim: 5320 14 
#> metadata(2): screenr_layout normalization
#> assays(1): counts
#> rownames(5320): 10208 10502 ... 13876 13995
#> rowData names(5): Barcode Gene Gene_ID Sequence Library
#> colnames(14): T1 T2 ... Time4_B Time4_c
#> colData names(5): Sample Day Treatment Group EffectiveLibSize
#> normalization: cpm (effective library sizes in colData(object)$EffectiveLibSize)
```

# Example annotation table

Annotation of the barcodes of
[`count_table`](https://emanuelsoda.github.io/ScreenR/reference/count_table.md),
an example loss-of-function shRNA screen. Each row describes one shRNA
(barcode) and the gene it targets. Use it together with
[`count_table`](https://emanuelsoda.github.io/ScreenR/reference/count_table.md)
to create a ScreenR object with
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Usage

``` r
data(annotation_table)
```

## Format

A data frame with 5320 rows (barcodes) and 5 columns:

- Gene:

  The name of the gene targeted by the shRNA.

- Barcode:

  The barcode identifier, unique for each shRNA. It links the annotation
  table to the count table.

- Gene_ID:

  A gene identifier (NA in this example).

- Sequence:

  The sequence of the shRNA (NA in this example).

- Library:

  The library of the shRNA, a pooled library from
  [Cellecta](https://cellecta.com/) (NA in this example).

## See also

[`count_table`](https://emanuelsoda.github.io/ScreenR/reference/count_table.md),
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Examples

``` r
data(annotation_table)
head(annotation_table)
#>       Gene Barcode Gene_ID Sequence Library
#> 1 Gene_380   10208      NA       NA      NA
#> 2 Gene_380   10502      NA       NA      NA
#> 3 Gene_380   10796      NA       NA      NA
#> 4 Gene_380   11089      NA       NA      NA
#> 5 Gene_380   11382      NA       NA      NA
#> 6 Gene_380   11675      NA       NA      NA
```

# Get the annotation table

Returns the annotation table of a ScreenR object, with the gene (and any
other information) of each barcode. A short summary of the size of the
table is printed.

The annotation of the barcodes of the object is its rowData, which is
what the analyses use; the returned table has those rows first, followed
by the rows of the annotation given by the user whose barcodes are not
in the count table (barcodes of the library that were not observed).

## Usage

``` r
get_annotation_table(object)

# S4 method for class 'screenr_object'
get_annotation_table(object)
```

## Arguments

- object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

The annotation table, a data frame with at least the columns Barcode and
Gene.

## See also

[`set_annotation_table`](https://emanuelsoda.github.io/ScreenR/reference/set_annotation_table.md),
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
annotation_table <- get_annotation_table(object)
#> ScreenR annotation table: 5320 rows, 5 columns
head(annotation_table)
#> # A tibble: 6 × 5
#>   Barcode Gene     Gene_ID Sequence Library
#>   <chr>   <chr>    <lgl>   <lgl>    <lgl>  
#> 1 10208   Gene_380 NA      NA       NA     
#> 2 10502   Gene_380 NA      NA       NA     
#> 3 10796   Gene_380 NA      NA       NA     
#> 4 11089   Gene_380 NA      NA       NA     
#> 5 11382   Gene_380 NA      NA       NA     
#> 6 11675   Gene_380 NA      NA       NA     
```

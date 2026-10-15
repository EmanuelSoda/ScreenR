# Get the annotation table

Returns the annotation table of a ScreenR object, with the gene (and any
other information) of each barcode. A short summary of the size of the
table is printed.

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
#>   Gene     Barcode Gene_ID Sequence Library
#>   <chr>    <fct>   <lgl>   <lgl>    <lgl>  
#> 1 Gene_380 10208   NA      NA       NA     
#> 2 Gene_380 10502   NA      NA       NA     
#> 3 Gene_380 10796   NA      NA       NA     
#> 4 Gene_380 11089   NA      NA       NA     
#> 5 Gene_380 11382   NA      NA       NA     
#> 6 Gene_380 11675   NA      NA       NA     
```

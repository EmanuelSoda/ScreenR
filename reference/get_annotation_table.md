# Get ScreenR annotation table

Get function for the annotation table of the ScreenR object

## Usage

``` r
get_annotation_table(object)

# S4 method for class 'screenr_object'
get_annotation_table(object)
```

## Arguments

- object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Value

The annotation table of the ScreenR object

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
annotation_table <- get_annotation_table(object)
#> ScreenR annotation table containing:
#>  5320 rows
#>  5 columns
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

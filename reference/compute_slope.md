# Compute Slope of a Gene

This function is used to compute the slope of the gene passed as input

## Usage

``` r
compute_slope(screenR_Object, genes, group_var)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

- genes:

  The genes for which the slope as to be computed. Those genes are the
  result of the three statistical methods selection

- group_var:

  The variable to use as independent variable (x) for the linear model

## Value

A tibble containing in each row the gene and the corresponding Slope

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

compute_slope(object,
    genes = c("Gene_42", "Gene_24"),
    group_var = c("T1", "T2", "TRT")
)
#> # A tibble: 2 × 2
#> # Rowwise:  Gene
#>   Gene     Slope
#>   <chr>    <dbl>
#> 1 Gene_24  0.131
#> 2 Gene_42 -1.98 
```

# Compute the variance explained by the principal components

Internal function that computes a principal component analysis of the
samples on the raw counts
([`prcomp`](https://rdrr.io/r/stats/prcomp.html)) and returns the
variance explained by each component. It is used by
[`plot_explained_variance`](https://emanuelsoda.github.io/ScreenR/reference/plot_explained_variance.md).

## Usage

``` r
compute_explained_variance(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

A data frame with the column Name ("Standard deviation", "Proportion of
Variance" and "Cumulative Proportion") and one column for each principal
component.

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

compute_explained_variance(object)
#>                     Name         PC1         PC2         PC3        PC4
#> 1     Standard deviation 27020.11492 17798.98865 10637.33427 9752.58832
#> 2 Proportion of Variance     0.43304     0.18791     0.06712    0.05642
#> 3  Cumulative Proportion     0.43304     0.62095     0.68807    0.74448
#>          PC5        PC6        PC7        PC8        PC9       PC10       PC11
#> 1 8498.64745 7915.02711 7755.60776 7359.95708 7126.65824 6787.07374 6274.56580
#> 2    0.04284    0.03716    0.03568    0.03213    0.03013    0.02732    0.02335
#> 3    0.78733    0.82448    0.86016    0.89229    0.92242    0.94974    0.97309
#>         PC12       PC13         PC14
#> 1 5190.01211 4293.13649 1.433355e-11
#> 2    0.01598    0.01093 0.000000e+00
#> 3    0.98907    1.00000 1.000000e+00
```

# Compute the variance explained by the principal components

Internal function that computes a principal component analysis of the
samples
([`compute_pca`](https://emanuelsoda.github.io/ScreenR/reference/compute_pca.md))
and returns the variance explained by each component. It is used by
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
#>                     Name      PC1      PC2      PC3      PC4      PC5      PC6
#> 1     Standard deviation 21.36921 14.37714 13.12902 12.59124 11.90008 10.51851
#> 2 Proportion of Variance  0.26264  0.11888  0.09914  0.09118  0.08145  0.06363
#> 3  Cumulative Proportion  0.26264  0.38152  0.48066  0.57185  0.65329  0.71693
#>        PC7      PC8      PC9    PC10    PC11     PC12     PC13         PC14
#> 1 10.26853 9.940392 9.230928 8.99261 7.68622 5.825269 5.369363 2.654333e-14
#> 2  0.06065 0.056830 0.049010 0.04651 0.03398 0.019520 0.016580 0.000000e+00
#> 3  0.77757 0.834400 0.883410 0.92992 0.96390 0.983420 1.000000 1.000000e+00
```

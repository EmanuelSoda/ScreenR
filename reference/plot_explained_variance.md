# Plot the variance explained by the principal components

Plots the proportion of the variance explained by each principal
component of a principal component analysis (PCA) of the samples,
computed on the log2 normalized counts (with the normalization chosen in
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md))
with
[`compute_explained_variance`](https://emanuelsoda.github.io/ScreenR/reference/compute_explained_variance.md).
It shows how many components are needed to describe the differences
between the samples. The samples on the components are shown by
[`plot_pca`](https://emanuelsoda.github.io/ScreenR/reference/plot_pca.md).

## Usage

``` r
plot_explained_variance(
  screenR_Object,
  cumulative = FALSE,
  color = "steelblue"
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- cumulative:

  Whether to plot the cumulative proportion of variance instead of the
  proportion of each component. Default FALSE.

- color:

  The fill color of the bars. Default "steelblue".

## Value

A ggplot object.

## See also

[`plot_pca`](https://emanuelsoda.github.io/ScreenR/reference/plot_pca.md),
[`plot_mds`](https://emanuelsoda.github.io/ScreenR/reference/plot_mds.md),
[`plot_replicate_correlation`](https://emanuelsoda.github.io/ScreenR/reference/plot_replicate_correlation.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

plot_explained_variance(object)


# Cumulative variance
plot_explained_variance(object, cumulative = TRUE)
```

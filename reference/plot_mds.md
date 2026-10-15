# Plot a multidimensional scaling of the samples

Plots the samples on a two-dimensional scatter plot so that the
distances on the plot approximate the typical log2 fold changes between
the samples ([`plotMDS`](https://rdrr.io/pkg/limma/man/plotMDS.html) on
the log2 counts per million of the raw counts). Replicates should
cluster together and the groups should separate.

## Usage

``` r
plot_mds(
  screenR_Object,
  groups = NULL,
  alpha = 0.8,
  size = 2.5,
  color = "black"
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- groups:

  An optional vector, with one value for each sample, used to fill the
  labels. By default the groups of the ScreenR object are used.

- alpha:

  The opacity of the labels, between 0 and 1. Default 0.8.

- size:

  The size of the labels. Default 2.5.

- color:

  The color of the label text. Default "black".

## Value

A ggplot object.

## See also

[`plot_replicate_correlation`](https://emanuelsoda.github.io/ScreenR/reference/plot_replicate_correlation.md),
[`plot_explained_variance`](https://emanuelsoda.github.io/ScreenR/reference/plot_explained_variance.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

plot_mds(object)
```

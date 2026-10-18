# Plot a multidimensional scaling of the samples

Plots the samples on a two-dimensional scatter plot so that the
distances on the plot approximate the typical log2 fold changes between
the samples ([`plotMDS`](https://rdrr.io/pkg/limma/man/plotMDS.html) on
the log2 normalized counts, see
[`log_normalized_counts`](https://emanuelsoda.github.io/ScreenR/reference/log_normalized_counts.md):
the normalization chosen in
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
is used). Replicates should cluster together and the groups should
separate.

## Usage

``` r
plot_mds(
  screenR_Object,
  groups = NULL,
  alpha = 0.8,
  size = 2.5,
  color = "black",
  palette = NULL
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- groups:

  What fills the labels: the name of a sample variable (for example
  "Treatment" or "Day", see
  [`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md)),
  or a vector with one value for each sample. By default the groups of
  the ScreenR object (Group).

- alpha:

  The opacity of the labels, between 0 and 1. Default 0.8.

- size:

  The size of the labels. Default 2.5.

- color:

  The color of the label text. Default "black".

- palette:

  An optional vector of colors for the values of groups, which replaces
  the palette of the object. By default, when groups is the name of a
  sample variable (or NULL, for Group), the palette of the object for
  that variable is used when it is set (see
  [`get_palettes`](https://emanuelsoda.github.io/ScreenR/reference/get_palettes.md)),
  otherwise the default colors of ggplot2.

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

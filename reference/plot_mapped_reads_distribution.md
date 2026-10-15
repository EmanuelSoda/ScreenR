# Plot the distribution of the mapped reads

Draws a box plot or a density plot of the number of reads of the
barcodes in each sample. It is a quality control plot: samples of good
quality have roughly the same distribution.

## Usage

``` r
plot_mapped_reads_distribution(
  screenR_Object,
  palette = NULL,
  alpha = 1,
  type = "boxplot"
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- palette:

  An optional vector of colors, one for each sample. By default the
  ggplot2 colors are used.

- alpha:

  The opacity of the boxes or densities, between 0 and 1. Default 1.

- type:

  The type of plot: "boxplot" (default) or "density".

## Value

A ggplot object.

## See also

[`count_mapped_reads`](https://emanuelsoda.github.io/ScreenR/reference/count_mapped_reads.md),
[`plot_mapped_reads`](https://emanuelsoda.github.io/ScreenR/reference/plot_mapped_reads.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

# Box plot
plot_mapped_reads_distribution(object)


# Density
plot_mapped_reads_distribution(object, type = "density")


# Density with transparency
plot_mapped_reads_distribution(object, type = "density", alpha = 0.2)
```

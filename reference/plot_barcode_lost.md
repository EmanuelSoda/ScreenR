# Plot the number of barcodes lost in each sample

Draws a bar plot of the number of barcodes with zero reads in each
sample, as computed by
[`barcode_lost`](https://emanuelsoda.github.io/ScreenR/reference/barcode_lost.md).
A sample that lost many more barcodes than the others usually has a low
quality.

## Usage

``` r
plot_barcode_lost(
  screenR_Object,
  palette = NULL,
  alpha = 1,
  legende_position = "none"
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- palette:

  An optional vector of colors for the samples (named by sample, or one
  for each sample in order), which replaces the palette of the object.
  By default the Sample palette of the object is used when it is set
  (see
  [`get_palettes`](https://emanuelsoda.github.io/ScreenR/reference/get_palettes.md)),
  otherwise the default colors of ggplot2.

- alpha:

  The opacity of the bars, between 0 and 1. Default 1.

- legende_position:

  The position of the legend: "none" (default), "top", "bottom", "right"
  or "left".

## Value

A ggplot object.

## See also

[`barcode_lost`](https://emanuelsoda.github.io/ScreenR/reference/barcode_lost.md),
[`plot_barcode_lost_for_gene`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_lost_for_gene.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
plot_barcode_lost(object)
```

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

  An optional vector of colors, one for each sample, used to fill the
  bars. By default the ggplot2 colors are used.

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

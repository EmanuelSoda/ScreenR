# Plot the Gini index of each sample

Plots the Gini index of each sample computed with
[`compute_gini`](https://emanuelsoda.github.io/ScreenR/reference/compute_gini.md):
0 means that all the barcodes have the same number of reads, values
close to 1 that a few barcodes take most of the reads. In a dropout
screen it usually increases over time; a strong increase or a large
difference between replicates can signal a bottleneck.

## Usage

``` r
plot_gini(screenR_Object, palette = NULL, alpha = 1, legende_position = "none")
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- palette:

  An optional vector of colors, one for each sample.

- alpha:

  The opacity of the bars, between 0 and 1. Default 1.

- legende_position:

  The position of the legend: "none" (default), "top", "bottom", "right"
  or "left".

## Value

A ggplot object.

## See also

[`compute_gini`](https://emanuelsoda.github.io/ScreenR/reference/compute_gini.md),
[`plot_barcode_lost`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_lost.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
plot_gini(object)
```

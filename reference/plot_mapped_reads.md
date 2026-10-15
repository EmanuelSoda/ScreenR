# Plot the number of mapped reads of each sample

Draws a bar plot of the total number of reads mapped to the barcodes in
each sample, computed with
[`mapped_reads`](https://emanuelsoda.github.io/ScreenR/reference/mapped_reads.md).
Samples with far fewer reads than the others may have a lower quality.

## Usage

``` r
plot_mapped_reads(
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

  The opacity of the bars, between 0 and 1. Default 1. It is used only
  when palette is given.

- legende_position:

  The position of the legend: "none" (default), "top", "bottom", "right"
  or "left".

## Value

A ggplot object.

## See also

[`mapped_reads`](https://emanuelsoda.github.io/ScreenR/reference/mapped_reads.md),
[`plot_mapped_reads_distribution`](https://emanuelsoda.github.io/ScreenR/reference/plot_mapped_reads_distribution.md),
[`plot_barcode_lost`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_lost.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

plot_mapped_reads(object)
```

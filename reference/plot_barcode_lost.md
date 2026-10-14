# Plot number of barcode lost

This function plots the number of barcode lost in each sample. Usually
lots of barcodes lost mean that the sample has low quality.

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

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

- palette:

  A vector of colors to be used to fill the barplot.

- alpha:

  A value for the opacity of the plot. Allowed values are in the range 0
  to 1

- legende_position:

  Where to positioning the legend of the plot. Allowed values are in the
  "top", "bottom", "right", "left", "none".

## Value

Returns the plot displaying the number of barcode lost in each sample

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

plot_barcode_lost(object)
```

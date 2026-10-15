# Plot the number of barcodes lost for each gene

Plots, for each gene and sample, the number of barcodes of the gene with
zero reads. Each gene is targeted by several shRNAs (barcodes), and a
gene needs enough barcodes left to give a robust result: this quality
control plot shows which genes lost many of their barcodes. Only the
genes that lost at least one barcode are shown.

## Usage

``` r
plot_barcode_lost_for_gene(screenR_Object, facet = TRUE, samples)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- facet:

  Whether to draw a separate panel for each sample (TRUE, default) or a
  single panel.

- samples:

  The names of the samples to plot (required).

## Value

A ggplot object with the number of lost barcodes on the x axis and the
genes on the y axis.

## See also

[`barcode_lost`](https://emanuelsoda.github.io/ScreenR/reference/barcode_lost.md),
[`plot_barcode_lost`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_lost.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

plot_barcode_lost_for_gene(object,
    samples = c("Time3_A", "Time3_B")
)

plot_barcode_lost_for_gene(object,
    samples = c("Time3_A", "Time3_B"),
    facet = FALSE
)
```

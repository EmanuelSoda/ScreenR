# Draw a barcode plot

Internal function that draws the barcode plot of
[`plot_barcode_hit`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md)
from the log2 fold changes of all the barcodes and the barcodes of the
gene.

## Usage

``` r
barcode_plot(
  log2fc,
  in_gene,
  title,
  quantile,
  labels,
  show_log2fc,
  span = 0.45
)
```

## Arguments

- log2fc:

  The log2 fold change of each barcode.

- in_gene:

  A logical vector, TRUE for the barcodes of the gene.

- title:

  The title of the plot.

- quantile:

  The two log2 fold change values that delimit the central region.

- labels:

  The labels of the left and right ends of the plot.

- show_log2fc:

  Whether to add the panel with the log2 fold changes.

- span:

  The span of the tricube moving average of the enrichment line (0.45,
  as in
  [`barcodeplot`](https://rdrr.io/pkg/limma/man/barcodeplot.html)).

## Value

A patchwork object.

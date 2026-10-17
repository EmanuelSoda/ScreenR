# Plot the barcodes of a gene (barcode plot)

Draws a barcode plot for a gene, as
[`barcodeplot`](https://rdrr.io/pkg/limma/man/barcodeplot.html) but with
ggplot2: all the barcodes are ranked by their log2 fold change (computed
with edgeR for the given contrast) and the barcodes of the gene are
shown as vertical bars. If most of the bars are on the left (negative
log2 fold change) the gene is depleted, if they are on the right it is
enriched. It is useful to check the hits of
[`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)
and
[`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md).

## Usage

``` r
plot_barcode_hit(
  screenR_Object,
  matrix_model,
  contrast,
  number_barcode = 3,
  gene,
  quantile = c(-0.5, 0.5),
  labels = c("Negative logFC", "Positive logFC"),
  show_log2fc = TRUE
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md),
  normalized with
  [`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md).
  The model uses its raw counts with its effective library sizes as
  offsets, the same normalization as the plots and the data table (see
  [`create_edger_obj`](https://emanuelsoda.github.io/ScreenR/reference/create_edger_obj.md)).

- matrix_model:

  The design matrix of the linear model, for example created with
  [`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md)
  (or with [`model.matrix`](https://rdrr.io/r/stats/model.matrix.html)).

- contrast:

  The comparison to plot: the name or index of a column of matrix_model,
  a numeric contrast vector (for example created with
  [`create_contrast`](https://emanuelsoda.github.io/ScreenR/reference/create_contrast.md))
  or a one-column matrix created with
  [`makeContrasts`](https://rdrr.io/pkg/limma/man/makeContrasts.html).

- number_barcode:

  The gene must have more than number_barcode barcodes (default 3), as
  in
  [`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md).

- gene:

  The name of the gene to plot.

- quantile:

  The two log2 fold change values that delimit the central (grey) region
  of the plot. Default c(-0.5, 0.5).

- labels:

  The labels of the left and right ends of the plot.

- show_log2fc:

  Whether to add the panel with the log2 fold change of all the barcodes
  (default TRUE).

## Value

A ggplot object (a patchwork of the panels), which can be printed, saved
with [`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html) or
combined with other plots. No theme is applied: add one to all the
panels with `&`, for example
`plot_barcode_hit(...) & ggplot2::theme_classic()`.

## Details

The plot has up to three panels, which share the ranking of the barcodes
on the x axis:

- the enrichment line: the density of the barcodes of the gene along the
  ranking divided by their average density (computed as in
  [`barcodeplot`](https://rdrr.io/pkg/limma/man/barcodeplot.html), with
  a tricube moving average). Values above 1 (dashed line) mean that the
  barcodes of the gene are concentrated there;

- the barcodes of the gene as vertical bars, colored by their log2 fold
  change, on three regions: below, between and above the two values of
  quantile;

- (when show_log2fc = TRUE) the log2 fold change of all the barcodes, in
  order, with the barcodes of the gene as points. It shows how strong
  the effect is, which the ranks alone do not.

## See also

[`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
[`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
[`plot_barcode_trend`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_trend.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
matrix_model <- create_design_matrix(object)
contrast <- create_contrast(matrix_model,
    treatment = "Treated", control = "Control"
)

plot_barcode_hit(object, matrix_model,
    contrast = contrast,
    gene = "Gene_300"
)


# Without the panel of the log2 fold changes
plot_barcode_hit(object, matrix_model,
    contrast = contrast,
    gene = "Gene_300", show_log2fc = FALSE
)
```

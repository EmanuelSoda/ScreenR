# Plot the barcodes of a gene (barcode plot)

Draws a [`barcodeplot`](https://rdrr.io/pkg/limma/man/barcodeplot.html)
for a gene: all the barcodes are ranked by their log2 fold change
(computed with edgeR for the given contrast) and the barcodes of the
gene are shown as vertical bars. If most of the bars are on the left
(negative log2 fold change) the gene is depleted, if they are on the
right it is enriched. It is useful to check the hits of
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
  labels = c("Negative logFC", "Positive logFC")
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

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

  The two log2 fold change values that delimit the central (shaded)
  region of the plot. Default c(-0.5, 0.5).

- labels:

  The labels of the left and right ends of the plot.

## Value

Draws the plot with base graphics and returns NULL invisibly.

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

#> NULL
```

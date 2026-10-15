# Plot the trend of genes over the samples

Plots, for the given genes, the mean normalized count of their barcodes
in each sample (taken from the data table), with a regression line. It
shows whether a hit decreases (or increases) over time in the selected
samples.

## Usage

``` r
plot_trend(
  screenR_Object,
  genes,
  group_var,
  alpha = 0.5,
  se = FALSE,
  point_size = 1,
  line_size = 1,
  nrow = 1,
  ncol = 1,
  scales = "free"
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- genes:

  The names of the genes to plot.

- group_var:

  The values of the Treatment column of the data table (see
  [`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md))
  of the samples to plot, for example the early time points and the
  treated samples.

- alpha:

  The opacity of the confidence band, between 0 and 1. Default 0.5.

- se:

  Whether to draw the confidence band of the regression line. Default
  FALSE.

- point_size:

  The size of the points. Default 1.

- line_size:

  The width of the regression line. Default 1.

- nrow:

  The number of rows of panels, when several genes are plotted.

- ncol:

  The number of columns of panels, when several genes are plotted.

- scales:

  The scales of the panels: "free" (default), "fixed", "free_x" or
  "free_y".

## Value

A ggplot object.

## See also

[`compute_slope`](https://emanuelsoda.github.io/ScreenR/reference/compute_slope.md),
[`filter_by_slope`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md),
[`plot_boxplot`](https://emanuelsoda.github.io/ScreenR/reference/plot_boxplot.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

plot_trend(object, genes = "Gene_42", group_var = c("T1", "T2", "TRT"))


plot_trend(object,
    genes = c("Gene_42", "Gene_100"),
    group_var = c("T1", "T2", "TRT"),
    nrow = 2
)
```

# Plot the normalized counts of genes in each sample

Draws, for the given genes, a box plot (or violin plot) of the
normalized counts of their barcodes in each sample, taken from the data
table. It shows the overall trend of a gene and whether it is depleted
or enriched.

## Usage

``` r
plot_boxplot(
  screenR_Object,
  genes,
  group_var,
  alpha = 0.5,
  nrow = NULL,
  ncol = NULL,
  fill_var = "Sample",
  type = "boxplot",
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
  of the samples to plot.

- alpha:

  Not used.

- nrow:

  The number of rows of panels, when several genes are plotted.

- ncol:

  The number of columns of panels, when several genes are plotted. By
  default (NULL) ggplot2 chooses nrow and ncol.

- fill_var:

  The column of the data table used to fill the boxes, for example
  "Sample" (default), "Day" or "Treatment".

- type:

  The type of plot: "boxplot" (default) or "violinplot".

- scales:

  The scales of the panels: "free" (default), "fixed", "free_x" or
  "free_y".

## Value

A ggplot object.

## See also

[`plot_trend`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md),
[`compute_data_table`](https://emanuelsoda.github.io/ScreenR/reference/compute_data_table.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

plot_boxplot(object,
    genes = c("Gene_34"),
    group_var = c("T1", "T2", "TRT"), nrow = 1, ncol = 2,
    fill_var = "Day", type = "violinplot"
)
```

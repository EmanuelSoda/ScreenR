# Plot Barcodes Hit

This function plots a boxplot for each sample for the genes passed as
input. It can be used to see the overall trend of a gene and so to
visualize if the gene is up or down regulated.

## Usage

``` r
plot_boxplot(
  screenR_Object,
  genes,
  group_var,
  alpha = 0.5,
  nrow = 1,
  ncol = 1,
  fill_var = "Sample",
  type = "boxplot",
  scales = "free"
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

- genes:

  The vector of genes that will be displayed

- group_var:

  The variable that as to be used to filter the data, for example the
  different treatment

- alpha:

  A value for the opacity of the plot. Allowed values are in the range 0
  to 1

- nrow:

  The number of rows in case multiple genes are plotted

- ncol:

  The number of columns in case multiple genes are plotted

- fill_var:

  The variable used to fill the boxplot

- type:

  The type of plot to use "boxplot" or "violinplot"

- scales:

  The scales used for the facet. Possible values can be "free", "fixed"
  and "free_y"

## Value

A boxplot

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

plot_boxplot(object,
    genes = c("Gene_34"),
    group_var = c("T1", "T2", "TRT"), nrow = 1, ncol = 2,
    fill_var = "Day", type = "violinplot"
)

```

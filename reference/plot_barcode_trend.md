# Plot the trend of the barcodes of genes over time

Plots the log2 fold change of each barcode of the given genes at the
different time points, one line for each barcode. It is useful to check
the efficacy of each shRNA: good shRNAs of a hit should have a
consistent trend over time.

## Usage

``` r
plot_barcode_trend(
  list_data_measure,
  genes,
  n_col = 1,
  size_line = 1,
  color = NULL
)
```

## Arguments

- list_data_measure:

  The tables of
  [`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
  for the different time points, as a list or already combined with
  [`bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html).
  The time points are shown in the order in which they appear.

- genes:

  The names of the genes to plot.

- n_col:

  The number of columns used to arrange the plots of the genes (default
  1). When larger than 1 the plots are combined with
  [`wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

- size_line:

  The width of the lines. Default 1.

- color:

  An optional vector of colors, one for each barcode.

## Value

With n_col = 1, a list with one ggplot object for each gene; with n_col
\> 1, a single patchwork object combining them.

## See also

[`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md),
[`plot_barcode_hit`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md),
[`find_discordant_barcodes`](https://emanuelsoda.github.io/ScreenR/reference/find_discordant_barcodes.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

metrics <- dplyr::bind_rows(
    compute_metrics(object,
        control = "Time3", treatment = "TRT",
        day = "Time3"
    ),
    compute_metrics(object,
        control = "Time4", treatment = "TRT",
        day = "Time4"
    )
)
# Multiple genes
plot_barcode_trend(metrics,
    genes = c("Gene_1", "Gene_50"),
    n_col = 2
)

# Single gene
plot_barcode_trend(metrics, genes = "Gene_300")
#> $Gene_300

#> 
```

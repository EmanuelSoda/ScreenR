# Plot the distribution of the barcode log2 fold changes

Draws the density of the log2 fold changes of the barcodes (treated
versus control), with one curve for each time point. It is useful to
compare the strength of the effect of the treatment at the different
time points.

## Usage

``` r
plot_zscore_distribution(time_point_measure, alpha = 1)
```

## Arguments

- time_point_measure:

  A list with the tables of
  [`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
  for the different time points (or a single table). Each table
  contains, for each barcode, the columns Log2FC and Day.

- alpha:

  The opacity of the densities, between 0 and 1. Default 1.

## Value

A ggplot object.

## See also

[`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md),
[`plot_barcode_trend`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_trend.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

table1 <- compute_metrics(object,
    control = "Time3", treatment = "TRT",
    day = "Time3"
)

table2 <- compute_metrics(object,
    control = "Time4", treatment = "TRT",
    day = "Time4"
)

plot_zscore_distribution(list(table1, table2), alpha = 0.5)
```

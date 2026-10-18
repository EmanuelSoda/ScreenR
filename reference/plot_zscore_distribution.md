# Plot the distribution of the barcode log2 fold changes

Draws the density of the log2 fold changes of the barcodes (treated
versus control), with one curve for each time point. It is useful to
compare the strength of the effect of the treatment at the different
time points.

## Usage

``` r
plot_zscore_distribution(time_point_measure, alpha = 1, palette = NULL)
```

## Arguments

- time_point_measure:

  A list with the tables of
  [`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
  for the different time points (or a single table). Each table
  contains, for each barcode, the columns Log2FC and Day.

- alpha:

  The opacity of the densities, between 0 and 1. Default 1.

- palette:

  An optional vector of colors for the time points (named by the values
  of the Day column, or one for each time point in order). The tables of
  [`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
  do not carry the palettes of the object: to use the same colors as the
  other plots, give the Day palette of the object,
  `get_palettes(object)$Day` (see
  [`get_palettes`](https://emanuelsoda.github.io/ScreenR/reference/get_palettes.md)).
  By default the colors of ggplot2.

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


# The same colors as the other plots of the object
object <- set_palettes(object,
    list(Day = c(T1 = "grey70", T2 = "grey40", Time3 = "#1B9E77",
        Time4 = "#D95F02")))
plot_zscore_distribution(list(table1, table2), alpha = 0.5,
    palette = get_palettes(object)$Day)
```

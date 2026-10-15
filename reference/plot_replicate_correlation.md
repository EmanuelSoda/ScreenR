# Plot the correlation between samples

Plots, as a heatmap, the correlation between all the pairs of samples,
computed on log2(CPM + 1) of the raw counts. Replicates of the same
condition are expected to be highly correlated; a replicate with a low
correlation to the others may be an outlier.

## Usage

``` r
plot_replicate_correlation(
  screenR_Object,
  method = c("pearson", "spearman"),
  label = TRUE
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- method:

  The correlation method: "pearson" (default) or "spearman".

- label:

  Whether to write the correlation value in each tile (default TRUE).

## Value

A ggplot object.

## See also

[`plot_mds`](https://emanuelsoda.github.io/ScreenR/reference/plot_mds.md),
[`plot_explained_variance`](https://emanuelsoda.github.io/ScreenR/reference/plot_explained_variance.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
plot_replicate_correlation(object)
```

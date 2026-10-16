# Filter hits by the slope of their trend

Refines a list of candidate hits (for example found by the statistical
methods) by their trend over time. For each gene it fits a regression
line of the normalized counts of its barcodes over the time points,
separately in the treated and in the control samples (see
[`compute_slope`](https://emanuelsoda.github.io/ScreenR/reference/compute_slope.md)),
and keeps the genes that decrease more in the treated samples than in
the controls.

## Usage

``` r
filter_by_slope(
  screenR_Object,
  genes,
  group_var_treatment,
  group_var_control,
  slope_control = NULL,
  slope_treatment = NULL,
  time_column = "Day",
  time = NULL
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- genes:

  The genes to filter, for example the hits found by
  [`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
  [`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)
  or
  [`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md).

- group_var_treatment:

  The values of the Treatment column of the data table (see
  [`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md))
  of the samples used for the treatment trend, for example the early
  time points and the treated samples.

- group_var_control:

  The values of the Treatment column of the data table of the samples
  used for the control trend.

- slope_control:

  An optional threshold: genes whose control slope is larger than this
  in absolute value are removed. Default NULL (no threshold).

- slope_treatment:

  An optional threshold: genes whose treatment slope is smaller than
  this in absolute value are removed. Default NULL (no threshold).

- time_column:

  The column of the data table with the time point of each sample: "Day"
  (default) or, for example, "Treatment" when the time points are
  described there. See
  [`compute_slope`](https://emanuelsoda.github.io/ScreenR/reference/compute_slope.md).

- time:

  An optional named numeric vector with the time of each time point, for
  example c(T0 = 0, T3 = 3, T18 = 18), so that the slopes are per unit
  of time. By default the time points are numbered 1, 2, ... in the
  order of the samples. See
  [`compute_slope`](https://emanuelsoda.github.io/ScreenR/reference/compute_slope.md).

## Value

A tibble with the columns Gene, slope_control and slope_treatment, with
one row for each gene that passes the filter.

## Details

A gene is kept when both slopes are negative and the treatment slope is
lower than the control slope, or when the control slope is zero or
positive and the treatment slope negative. Before this, the optional
thresholds remove the genes whose control slope is larger in absolute
value than slope_control and those whose treatment slope is smaller in
absolute value than slope_treatment.

## See also

[`compute_slope`](https://emanuelsoda.github.io/ScreenR/reference/compute_slope.md),
[`filter_by_variance`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_variance.md),
[`plot_trend`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

filter_by_slope(
    screenR_Object = object, genes = c("Gene_1", "Gene_2"),
    group_var_treatment = c("T1", "T2", "TRT"),
    group_var_control = c("T1", "T2", "Time3", "Time4"),
    slope_control = 0.5, slope_treatment = 1
)
#> # A tibble: 0 × 3
#> # ℹ 3 variables: Gene <chr>, slope_control <dbl>, slope_treatment <dbl>
```

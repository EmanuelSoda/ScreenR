# Compute the slope of the trend of genes

Computes, for each gene, the slope of a regression line of the
normalized counts (Frequency column of the data table) of its barcodes
over time. Only the samples whose Treatment is in group_var are used. It
is used by
[`filter_by_slope`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md),
and it is the slope of the line drawn by
[`plot_trend`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md).

## Usage

``` r
compute_slope(
  screenR_Object,
  genes,
  group_var,
  time_column = "Day",
  time = NULL
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md),
  normalized with
  [`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
  (its normalized counts are used).

- genes:

  The genes for which the slope is computed.

- group_var:

  The values of the Treatment column of the data table (see
  [`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md))
  of the samples to use.

- time_column:

  The column of the data table with the time point of each sample: "Day"
  (default) or, for example, "Treatment" when the time points are
  described there.

- time:

  An optional named numeric vector with the time of each time point (the
  names are the values of time_column), for example c(T0 = 0, T3 = 3,
  T18 = 18). By default the time points are numbered 1, 2, ... in the
  order of the samples.

## Value

A row-wise tibble with the columns Gene and Slope, with one row for each
gene.

## Details

The time of each sample is taken from its time point, in the time_column
column of the data table, so replicates of the same time point share the
same time. By default the time points are numbered 1, 2, ... in the
order of the samples, so they are treated as equally spaced; give time
to use the real times (for example days 0, 3 and 18), and the slope is
then per unit of time.

## See also

[`filter_by_slope`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md),
[`plot_trend`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

compute_slope(object,
    genes = c("Gene_42", "Gene_24"),
    group_var = c("T1", "T2", "TRT")
)
#> # A tibble: 2 × 2
#> # Rowwise:  Gene
#>   Gene     Slope
#>   <chr>    <dbl>
#> 1 Gene_24 -0.597
#> 2 Gene_42 -1.33 

# With the real times of the time points
compute_slope(object,
    genes = c("Gene_42", "Gene_24"),
    group_var = c("T1", "T2", "TRT"),
    time = c(T1 = 0, T2 = 2, Time3 = 7, Time4 = 14)
)
#> # A tibble: 2 × 2
#> # Rowwise:  Gene
#>   Gene      Slope
#>   <chr>     <dbl>
#> 1 Gene_24 -0.203 
#> 2 Gene_42 -0.0409
```

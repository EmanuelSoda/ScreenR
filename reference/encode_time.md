# Encode the time of the samples

Internal function that gives each row of the data table the time of its
sample, used by
[`compute_slope`](https://emanuelsoda.github.io/ScreenR/reference/compute_slope.md)
and
[`plot_trend`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md).
Replicates of the same time point share the same time.

## Usage

``` r
encode_time(data, time_column = "Day", time = NULL)
```

## Arguments

- data:

  The data table, or a part of it, with the columns Sample and
  time_column.

- time_column:

  The column with the time point of each sample.

- time:

  NULL to number the time points 1, 2, ... in the order of the samples,
  or a named numeric vector with the time of each time point.

## Value

A numeric vector with one value for each row of data, with the attribute
"labels" (the time point of each distinct time, in order) when time is
NULL.

# Compute the trend of genes

Internal function that computes, for each gene and sample, the mean
normalized count of the barcodes of the gene, and the time of the sample
(see
[`encode_time`](https://emanuelsoda.github.io/ScreenR/reference/encode_time.md)).

## Usage

``` r
compute_trend(
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
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- genes:

  The names of the genes.

- group_var:

  The values of the Treatment column of the samples to use.

- time_column:

  The column of the data table with the time point of each sample.

- time:

  NULL or a named numeric vector with the time of each time point.

## Value

A tibble with the columns Sample, Gene, Frequency and Time.

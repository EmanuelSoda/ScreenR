# Log2 normalized counts

Internal function that returns the log2 of the normalized counts plus 1,
used by
[`plot_pca`](https://emanuelsoda.github.io/ScreenR/reference/plot_pca.md),
[`plot_explained_variance`](https://emanuelsoda.github.io/ScreenR/reference/plot_explained_variance.md)
and
[`plot_mds`](https://emanuelsoda.github.io/ScreenR/reference/plot_mds.md).
The normalized counts are those computed by
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md),
with the method chosen there ("cpm", "tmm" or "control"); if the counts
have not been normalized, counts per million are used, as
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
does by default. On the raw counts the first principal component would
mostly follow the sequencing depth of the samples. Barcodes with no
reads in any sample are left out.

## Usage

``` r
log_normalized_counts(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

A numeric matrix with one row for each barcode and one column for each
sample.

# Log2 normalized counts

Internal function that returns the log2 of the normalized counts plus 1,
used by
[`plot_pca`](https://emanuelsoda.github.io/ScreenR/reference/plot_pca.md),
[`plot_explained_variance`](https://emanuelsoda.github.io/ScreenR/reference/plot_explained_variance.md)
and
[`plot_mds`](https://emanuelsoda.github.io/ScreenR/reference/plot_mds.md).
The normalized counts are the raw counts divided by the effective
library sizes set by
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
("cpm", "tmm" or "control"), so the object must be normalized; the log
and the + 1 are display transformations. On the raw counts the first
principal component would mostly follow the sequencing depth of the
samples. Barcodes with no reads in any sample are left out.

## Usage

``` r
log_normalized_counts(screenR_Object, what = "This function")
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- what:

  What needs the normalized counts, for the error message.

## Value

A numeric matrix with one row for each barcode and one column for each
sample.

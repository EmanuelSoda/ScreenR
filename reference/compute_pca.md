# Principal component analysis of the samples

Internal function that computes the principal component analysis
([`prcomp`](https://rdrr.io/r/stats/prcomp.html)) of the samples on the
log2 normalized counts
([`log_normalized_counts`](https://emanuelsoda.github.io/ScreenR/reference/log_normalized_counts.md)).

## Usage

``` r
compute_pca(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

A prcomp object, with one row for each sample in x.

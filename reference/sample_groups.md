# Groups of the Samples of a Plot

Internal function used by
[`plot_mds`](https://emanuelsoda.github.io/ScreenR/reference/plot_mds.md)
and
[`plot_pca`](https://emanuelsoda.github.io/ScreenR/reference/plot_pca.md):
it returns the values that fill the labels of the samples, and the
sample variable they come from (whose palette is used).

## Usage

``` r
sample_groups(object, groups)
```

## Arguments

- object:

  A ScreenR object.

- groups:

  NULL (the Group of the samples), the name of a sample variable, or a
  vector with one value for each sample.

## Value

A list with values, variable (NULL for a vector) and label (the title of
the legend).

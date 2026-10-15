# Scores of Random Barcode Sets

Internal function that computes the mean or the median of n random sets
of size barcodes drawn from the reference scores.

## Usage

``` r
random_set_scores(reference, size, n, statistic)
```

## Arguments

- reference:

  The scores of the reference barcodes.

- size:

  The number of barcodes of each set.

- n:

  The number of random sets.

- statistic:

  "mean" or "median".

## Value

A numeric vector of length n.

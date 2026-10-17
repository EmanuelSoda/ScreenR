# Remove a Stale Normalization

Internal function called after the assays are replaced: when the raw
counts have changed, the effective library sizes no longer describe
them, so the normalization is removed (with a message). The object is
then checked.

## Usage

``` r
counts_replaced(x, old_counts)
```

## Arguments

- x:

  The ScreenR object after the replacement.

- old_counts:

  The counts before the replacement.

## Value

The ScreenR object.

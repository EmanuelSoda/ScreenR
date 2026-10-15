# Match a Table to the Counts

Internal function that puts the values of a table with a Barcode column
in a matrix with the same rows and columns as the count matrix (NA where
a barcode or sample is missing).

## Usage

``` r
match_to_counts(table, counts)
```

## Arguments

- table:

  A table with a Barcode column and one column per sample.

- counts:

  The count matrix.

## Value

A numeric matrix with the dimensions of counts.

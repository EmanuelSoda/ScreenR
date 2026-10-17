# Effective Library Sizes of a Normalized Table

Internal function used by
[`set_normalized_count_table`](https://emanuelsoda.github.io/ScreenR/reference/set_normalized_count_table.md):
it checks that a normalized count table is the raw counts scaled by one
factor for each sample, and returns the corresponding effective library
sizes (raw / normalized \* 1e6).

## Usage

``` r
sizes_from_normalized_table(object, table)
```

## Arguments

- object:

  A ScreenR object.

- table:

  A table with a Barcode column and one column for each sample.

## Value

The effective library size of each sample.

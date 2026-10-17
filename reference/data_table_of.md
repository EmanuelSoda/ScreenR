# Build the Data Table

Internal function that builds the data table of a normalized object: one
row for each barcode and sample, with the columns Barcode, Gene, Sample,
Frequency (the normalized count, raw count / effective library size \*
1e6), the Sequence, Library and Gene_ID columns of the annotation when
present, Day and Treatment. It is computed from the assays, rowData and
colData each time and never stored.

## Usage

``` r
data_table_of(object, what = "This function")
```

## Arguments

- object:

  A ScreenR object of the current layout.

- what:

  What needs the data table, for the error message.

## Value

A tibble.

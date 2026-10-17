# Barcode Identifiers

Internal function that converts barcode identifiers to character, the
type used for the row names of the object. Factors are converted to
their labels and numbers to their decimal representation, without
scientific notation (10208 and "10208" give the same identifier, 1e5
gives "100000").

## Usage

``` r
as_barcode_id(x, what = "barcode identifiers", allow_missing = FALSE)
```

## Arguments

- x:

  The identifiers.

- what:

  A description of the identifiers, for the error message.

- allow_missing:

  When FALSE (default), missing or empty identifiers are an error.

## Value

A character vector.

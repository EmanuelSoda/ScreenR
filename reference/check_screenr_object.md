# Check a ScreenR Object

Internal validity method of the screenr_object class: the barcode
identifiers must be unique and not missing, the Barcode column of the
rowData must equal them, the counts must be finite and not negative, and
the normalization must be complete (effective library sizes positive and
finite, and recorded in the metadata).

## Usage

``` r
check_screenr_object(object)
```

## Arguments

- object:

  A ScreenR object.

## Value

TRUE or a character vector describing the problems.

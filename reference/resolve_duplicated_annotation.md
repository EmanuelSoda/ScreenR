# Resolve Duplicated Annotation

Internal function that merges the rows of the annotation that are exact
copies, and stops when a barcode is annotated more than once with
different values (unless policy is "first", which keeps the first row of
each barcode).

## Usage

``` r
resolve_duplicated_annotation(annotation, policy = "error")
```

## Arguments

- annotation:

  The annotation table, with a character Barcode column.

- policy:

  "error" or "first".

## Value

The annotation table with one row for each barcode.

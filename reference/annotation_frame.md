# Annotation Table with Character Identifiers

Internal function that returns the annotation table as a data frame with
the barcode identifiers as character (an empty data frame with a Barcode
column when there is no annotation).

## Usage

``` r
annotation_frame(annotation)
```

## Arguments

- annotation:

  The annotation table or NULL.

## Value

A tibble.

# Annotation of Observed Barcodes

Internal function that builds the rowData of the object: the annotation
of each barcode, matched by identifier (NA for the barcodes missing from
the annotation).

## Usage

``` r
annotation_for(identifiers, annotation)
```

## Arguments

- identifiers:

  The barcode identifiers (row names of the counts).

- annotation:

  The annotation, as returned by annotation_frame.

## Value

A DataFrame with the Barcode column and the annotation columns.

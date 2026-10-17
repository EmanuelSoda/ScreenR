# Check the Annotation Table

Internal function that finds and renames the barcode and gene columns of
the annotation table, converts the identifiers to character and resolves
(or rejects) barcodes annotated more than once.

## Usage

``` r
check_annotation_table(
  annotation,
  barcode_column,
  gene_column,
  duplicate_annotation = "error"
)
```

## Arguments

- annotation:

  The annotation table.

- barcode_column:

  The name of the barcode column.

- gene_column:

  The name of the gene column.

- duplicate_annotation:

  "error" or "first".

## Value

The checked annotation table.

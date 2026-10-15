# Check the Annotation Table

Internal function that finds and renames the barcode and gene columns of
the annotation table and warns about duplicated barcodes.

## Usage

``` r
check_annotation_table(annotation, barcode_column, gene_column)
```

## Arguments

- annotation:

  The annotation table.

- barcode_column:

  The name of the barcode column.

- gene_column:

  The name of the gene column.

## Value

The checked annotation table.

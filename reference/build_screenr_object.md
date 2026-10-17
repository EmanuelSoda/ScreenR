# Build a ScreenR Object

Internal function that builds a screenr_object from tables already
checked by
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md):
the counts become the counts assay (with the barcode identifiers as row
names), the annotation of the observed barcodes the rowData and the
sample information the colData.

## Usage

``` r
build_screenr_object(
  count_table = NULL,
  annotation_table = NULL,
  groups = NULL,
  replicates = c(""),
  sample_table = NULL,
  column_names = list()
)
```

## Arguments

- count_table:

  A table with a Barcode column and one numeric column for each sample,
  without duplicated barcodes.

- annotation_table:

  The annotation table, with a Barcode column and no conflicting
  duplicated barcodes, or NULL.

- groups:

  The groups of the samples.

- replicates:

  The replicate labels: one for each sample, or any other value (kept
  for compatibility).

- sample_table:

  An optional sample table with the columns Sample, Day and Treatment,
  in the order of the samples; when NULL the sample names are parsed.

- column_names:

  The original column names.

## Value

A screenr_object.

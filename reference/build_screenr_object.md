# Build a ScreenR Object

Internal function that builds a screenr_object from the tables, without
checking them (the checks are done by
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)).

## Usage

``` r
build_screenr_object(
  count_table = NULL,
  annotation_table = NULL,
  groups = NULL,
  replicates = c(""),
  normalized_count_table = NULL,
  data_table = NULL,
  sample_table = NULL,
  column_names = list()
)
```

## Arguments

- count_table:

  A table with a Barcode column and one column for each sample.
  Non-numeric columns other than Barcode are ignored.

- annotation_table:

  The annotation table, with a Barcode column.

- groups:

  The groups of the samples.

- replicates:

  The replicates.

- normalized_count_table:

  An optional normalized count table, with a Barcode column; it is
  matched to the count table by barcode and sample.

- data_table:

  The data table.

- sample_table:

  An optional sample table with the columns Sample, Day and Treatment,
  in the order of the samples; when NULL the sample names are parsed.

- column_names:

  The original column names.

## Value

A screenr_object.

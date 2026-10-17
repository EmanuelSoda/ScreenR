# Internal Accessors

Internal functions that return views of a ScreenR object as tables: they
are built from the assays, rowData and colData each time and are never
stored. `count_table_of` and `normalized_table_of` are for export and
the public getters: the computations use the assays directly.

## Usage

``` r
count_table_of(object)

normalized_table_of(object)

assay_table(object, assay_name, column_names = NULL)

annotation_of(object)

genes_of(object)

groups_of(object)

sample_table_of(object)
```

## Arguments

- object:

  A ScreenR object.

- assay_name:

  The name of the assay.

- column_names:

  Ignored (kept for older objects).

## Value

A tibble (count_table_of, normalized_table_of, sample_table_of,
annotation_of), or a factor (groups_of).

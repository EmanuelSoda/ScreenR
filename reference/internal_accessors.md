# Internal Accessors

Internal functions that return the tables of a ScreenR object in the
format used by the rest of the package, for objects of the current and
of older versions.

## Usage

``` r
count_table_of(object)

normalized_table_of(object)

assay_table(object, assay_name)

annotation_of(object)

groups_of(object)

sample_table_of(object)
```

## Arguments

- object:

  A ScreenR object.

- assay_name:

  The name of the assay.

## Value

A tibble (count_table_of, normalized_table_of, sample_table_of), a data
frame (annotation_of) or a factor (groups_of).

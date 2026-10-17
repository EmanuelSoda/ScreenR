# Rebuild an Older ScreenR Object

Internal function used by
[`update_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/update_screenr_object.md)
to build a current object from the tables of an older one.

## Usage

``` r
rebuild_screenr_object(
  count_table,
  annotation_table,
  groups,
  replicates,
  sample_table,
  column_names,
  metadata = list()
)
```

## Arguments

- count_table, annotation_table, groups, replicates, sample_table,
  column_names:

  The tables of the older object.

- metadata:

  The metadata of the older object, if any.

## Value

A screenr_object of the current layout, not normalized.

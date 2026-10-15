# Rename a Column

Internal function that renames a column to its internal name, checking
that the internal name is not already used by another column.

## Usage

``` r
rename_column(table, from, to, table_name)
```

## Arguments

- table:

  The table.

- from:

  The current column name.

- to:

  The internal column name.

- table_name:

  The name of the table, for the error message.

## Value

The table with the column renamed.

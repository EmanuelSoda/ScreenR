# Find a Column

Internal function that returns the name of a column of a table, or stops
with an error that suggests the most similar column name and the
argument to use.

## Usage

``` r
find_column(table, wanted, default, table_name, argument)
```

## Arguments

- table:

  The table.

- wanted:

  The column name given by the user.

- default:

  The default (internal) column name.

- table_name:

  The name of the table, for the error message.

- argument:

  The name of the argument, for the error message.

## Value

The column name.

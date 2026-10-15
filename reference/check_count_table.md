# Check the Count Table

Internal function that finds and renames the barcode column of the count
table, converts sample columns stored as text, removes the other
non-numeric columns and warns about duplicated barcodes.

## Usage

``` r
check_count_table(table, barcode_column)
```

## Arguments

- table:

  The count table.

- barcode_column:

  The name of the barcode column.

## Value

The checked count table, with the barcode column first.

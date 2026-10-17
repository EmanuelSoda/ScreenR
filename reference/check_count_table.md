# Check the Count Table

Internal function that finds and renames the barcode column of the count
table, converts the identifiers to character, converts sample columns
stored as text, removes the other non-numeric columns, rejects invalid
counts and resolves (or rejects) duplicated barcodes.

## Usage

``` r
check_count_table(table, barcode_column, duplicate_barcodes = "error")
```

## Arguments

- table:

  The count table.

- barcode_column:

  The name of the barcode column.

- duplicate_barcodes:

  "error", "sum" or "first".

## Value

The checked count table, with the barcode column first.

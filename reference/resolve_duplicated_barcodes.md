# Resolve Duplicated Barcodes

Internal function that stops when barcodes appear more than once in the
count table, or combines their rows when a policy is given.

## Usage

``` r
resolve_duplicated_barcodes(table, policy = "error")
```

## Arguments

- table:

  The count table, with a character Barcode column.

- policy:

  "error", "sum" (add up the counts) or "first".

## Value

The count table without duplicated barcodes.

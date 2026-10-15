# Gene Log2 Fold Change

Internal function that summarises the barcode log2 fold changes to a
gene log2 fold change using the median.

## Usage

``` r
gene_log2fc(log2fc, genes)
```

## Arguments

- log2fc:

  The log2 fold change of each barcode.

- genes:

  The gene of each barcode.

## Value

A tibble with the columns Gene and Log2FC.

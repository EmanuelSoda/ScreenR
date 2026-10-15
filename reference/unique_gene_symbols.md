# Barcodes of each gene

Internal function that returns, for each gene with more than
number_barcode barcodes, the indices of its barcodes.

## Usage

``` r
unique_gene_symbols(gene_symbols, number_barcode = 3)
```

## Arguments

- gene_symbols:

  The gene of each barcode.

- ...:

  Arguments passed on to
  [`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)

  `number_barcode`

  :   Only genes with more than number_barcode barcodes are tested
      (default 3, so a gene needs at least 4 barcodes). The barcodes of
      a gene are tested together as a set, so this is not a number of
      barcodes that must change.

## Value

A named list with the indices of the barcodes of each gene.

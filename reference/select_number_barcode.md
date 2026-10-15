# Select the barcodes of a gene

Internal function that returns the indices of the barcodes of a gene,
when the gene has more than number_barcode barcodes.

## Usage

``` r
select_number_barcode(gene, gene_symbols, number_barcode)
```

## Arguments

- gene:

  The gene name

- ...:

  Arguments passed on to
  [`unique_gene_symbols`](https://emanuelsoda.github.io/ScreenR/reference/unique_gene_symbols.md)

  `gene_symbols`

  :   The gene of each barcode.

## Value

The indices of the barcodes of the gene passed as input, or NULL if the
gene has no more than number_barcode barcodes

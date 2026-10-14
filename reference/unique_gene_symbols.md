# Unique gene Symbols

Compute a unique gene symbol for gene

## Usage

``` r
unique_gene_symbols(gene_symbols, number_barcode = 3)
```

## Arguments

- gene_symbols:

  The gene symbols list

- ...:

  Arguments passed on to
  [`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)

  `number_barcode`

  :   Number of barcode that as to be differentially expressed (DE)in
      order to consider the gene associated DE. Example a gene is
      associated with 10 shRNA we consider a gene DE if it has at least
      number_barcode = 5 shRNA DE.

## Value

A list of unique gene symbols

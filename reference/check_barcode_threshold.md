# Check a Threshold on the Number of Barcodes

Warns when a threshold on the number of barcodes per gene excludes more
than max_excluded of the genes, for example number_barcode = 3 (genes
need more than 3 barcodes) with a CRISPR library of 4 sgRNAs per gene in
which some sgRNAs were filtered out. The warning reports the median
number of barcodes per gene and suggests a threshold that keeps most of
the genes.

## Usage

``` r
check_barcode_threshold(
  n_barcodes,
  threshold,
  strict = TRUE,
  argument = "number_barcode",
  max_excluded = 0.2
)
```

## Arguments

- n_barcodes:

  The number of barcodes of each gene.

- threshold:

  The threshold.

- strict:

  TRUE when genes need more than threshold barcodes (number_barcode),
  FALSE when they need at least threshold barcodes (min_barcodes).

- argument:

  The name of the argument, used in the message.

- max_excluded:

  The fraction of excluded genes above which a warning is given.

## Value

Invisibly, the fraction of excluded genes.

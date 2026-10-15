# Barcode Log2 Fold Change

Internal function that fits the edgeR negative binomial model and
returns the log2 fold change of each barcode for the given contrast.

## Usage

``` r
barcode_log2fc(xglm, design, contrast)
```

## Arguments

- xglm:

  A DGEList with the dispersion estimated.

- design:

  The design matrix.

- contrast:

  A contrast checked with `as_contrast`.

## Value

A numeric vector with the log2 fold change of each barcode.

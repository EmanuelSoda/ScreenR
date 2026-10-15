# Compute Camera

This internal function computes the actual hits using the camera method.

## Usage

``` r
compute_camera(xglm, DGEList, matrix_model, contrast, number_barcode = 3)
```

## Arguments

- xglm:

  object created with
  [`estimateDisp`](https://rdrr.io/pkg/edgeR/man/estimateDisp.html)

- DGEList:

  edgeR object

- ...:

  Arguments passed on to
  [`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)

  `matrix_model`

  :   The design matrix of the linear model, for example created with
      [`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md)
      (or with
      [`model.matrix`](https://rdrr.io/r/stats/model.matrix.html)).

  `contrast`

  :   The comparison to test: the name or index of a column of
      matrix_model, a numeric contrast vector (for example created with
      [`create_contrast`](https://emanuelsoda.github.io/ScreenR/reference/create_contrast.md))
      or a one-column matrix created with
      [`makeContrasts`](https://rdrr.io/pkg/limma/man/makeContrasts.html).

  `number_barcode`

  :   Only genes with more than number_barcode barcodes are tested
      (default 3, so a gene needs at least 4 barcodes). The barcodes of
      a gene are tested together as a set, so this is not a number of
      barcodes that must change.

## Value

The list of hits found by the camera method

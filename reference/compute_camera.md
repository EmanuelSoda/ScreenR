# Compute Camera

This internal function computes the actual hits using the camera method.

## Usage

``` r
compute_camera(
  xglm,
  DGEList,
  matrix_model,
  contrast,
  number_barcode = 3,
  thresh = 1e-04,
  lfc = 1
)
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

  :   The matrix that will be used to perform the linear model analysis
      created using
      [`model.matrix`](https://rdrr.io/r/stats/model.matrix.html)

  `thresh`

  :   The threshold for the False Discovery Rate (FDR) that has to be
      used to select the statistically significant hits.

  `lfc`

  :   The Log2FC threshold.

  `number_barcode`

  :   Number of barcode that as to be differentially expressed (DE)in
      order to consider the gene associated DE. Example a gene is
      associated with 10 shRNA we consider a gene DE if it has at least
      number_barcode = 5 shRNA DE.

## Value

The list of hits found by the camera method

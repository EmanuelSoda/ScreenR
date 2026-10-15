# Run CAMERA

Internal function that runs
[`camera`](https://rdrr.io/pkg/limma/man/camera.html) and always returns
an FDR column (camera omits it when only one gene is tested), with a
clear error when no gene can be tested.

## Usage

``` r
run_camera(xglm, index, design, contrast)
```

## Arguments

- xglm:

  A DGEList with the dispersion estimated.

- index:

  The list of barcode indices of each gene.

- design:

  The design matrix.

- contrast:

  A contrast checked with `as_contrast`.

## Value

The data frame returned by camera, with an FDR column.

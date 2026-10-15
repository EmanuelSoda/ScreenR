# Check a Contrast

Internal function that converts a one-column contrast matrix (as
returned by
[`makeContrasts`](https://rdrr.io/pkg/limma/man/makeContrasts.html)) to
a vector, and checks the contrast against the design.

## Usage

``` r
as_contrast(contrast, design)
```

## Arguments

- contrast:

  A column name or index of the design, a numeric vector, or a
  one-column contrast matrix.

- design:

  The design matrix.

## Value

The contrast as a single name/index or a numeric vector.

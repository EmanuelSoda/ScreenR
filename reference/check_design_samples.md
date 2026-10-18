# Check Design Sample Alignment

Internal validation that model rows identify the samples of the object
in exactly the same order. No positional guessing is used.

## Usage

``` r
check_design_samples(design, object)
```

## Arguments

- design:

  A design matrix with sample identifiers as row names.

- object:

  A ScreenR object.

## Value

The validated design matrix.

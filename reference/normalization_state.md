# Normalization State

Internal functions that read the normalization of an object:
`is_normalized` tells whether it is normalized, `effective_lib_size`
returns the effective library size of each sample (stopping with an
informative error when the object is not normalized) and
`normalized_counts` the normalized counts, raw counts / effective
library size \* 1e6.

## Usage

``` r
is_normalized(object)

effective_lib_size(object, what = "This function")

normalized_counts(object, what = "This function")
```

## Arguments

- object:

  A ScreenR object (of the current layout).

- what:

  What needs the normalization, for the error message.

## Value

TRUE/FALSE, a named numeric vector or a numeric matrix.

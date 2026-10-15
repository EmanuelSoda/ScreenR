# Gene Set Test Result in One Direction

Internal function that turns the result of a gene set test (camera,
mroast or fry) into one-sided p-values for the requested direction,
computes the FDR over all the genes and keeps the genes changing in that
direction.

## Usage

``` r
one_direction(result, direction, fdr_method)
```

## Arguments

- result:

  The data frame of the test, with the columns Gene, Direction and
  PValue.

- direction:

  The requested direction ("Down" or "Up").

- fdr_method:

  "BH" or "storey".

## Value

The result with one-sided PValue and FDR, only for the genes in the
requested direction.

# Membership of the Genes

Internal function that returns, for each gene found by at least one
method, the combination of methods that find it.

## Usage

``` r
hit_membership(sets)
```

## Arguments

- sets:

  A named list of gene sets.

## Value

A tibble with the columns Gene and Pattern (a string of 0 and 1, one
character for each method) and one logical column per method.

# Hit Sets of Several Methods

Internal function that turns the results of several hit-finding methods
into named sets of genes, used by
[`find_common_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_common_hit.md)
and
[`plot_common_hit`](https://emanuelsoda.github.io/ScreenR/reference/plot_common_hit.md).

## Usage

``` r
hit_sets(results, fdr = NULL, expressions = NULL)
```

## Arguments

- results:

  A list of results: gene-level tables with a Gene column, or character
  vectors of gene names.

- fdr:

  NULL to use all the genes of each result, or an FDR threshold applied
  to the results that have an FDR column.

- expressions:

  The expressions given for the results, used to label the unnamed ones.

## Value

A named list of character vectors (unique gene names).

# Axis label of an MDS plot

Internal function that returns the label of a dimension of the MDS plot:
limma computes the distances from the leading log2 fold changes, and the
label gives the proportion of the variance explained by the dimension
when limma returns it.

## Usage

``` r
mds_axis_label(mds, dimension)
```

## Arguments

- mds:

  The result of [`plotMDS`](https://rdrr.io/pkg/limma/man/plotMDS.html).

- dimension:

  The dimension (1 or 2).

## Value

A character string.

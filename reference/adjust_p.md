# Adjust P-values

Internal function that computes the False Discovery Rate from the
p-values, with the Benjamini-Hochberg method or with the Storey method,
which multiplies the Benjamini-Hochberg values by the estimated
proportion of genes without effect (pi0, estimated as the proportion of
p-values above 0.5 divided by 0.5). The Storey method is less
conservative when many genes have an effect.

## Usage

``` r
adjust_p(p, method = c("BH", "storey"))
```

## Arguments

- p:

  The p-values.

- method:

  "BH" or "storey".

## Value

The adjusted p-values.

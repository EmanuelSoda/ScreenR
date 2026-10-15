# One-sided P-values

Internal function that converts the two-sided p-values of
[`camera`](https://rdrr.io/pkg/limma/man/camera.html),
[`mroast`](https://rdrr.io/pkg/limma/man/roast.html) and
[`fry`](https://rdrr.io/pkg/limma/man/roast.html) (which come with the
direction of the change) into one-sided p-values for the requested
direction: p / 2 for the genes changing in that direction and 1 - p / 2
for the others.

## Usage

``` r
one_sided_p(p, observed, direction)
```

## Arguments

- p:

  The two-sided p-values.

- observed:

  The direction of each gene ("Up" or "Down").

- direction:

  The requested direction.

## Value

The one-sided p-values.

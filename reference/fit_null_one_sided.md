# Fit the empirical null on the side without hits

Internal function: the centre of the null is the mode of x and its
spread is estimated from the values on the side without hits only (for a
normal distribution the median distance from the mean is 0.674 standard
deviations).

## Usage

``` r
fit_null_one_sided(x, side = "right")
```

## Arguments

- x:

  The gene statistics.

- side:

  "right" (the hits are depleted, on the left) or "left".

## Value

A named vector with mean and sd.

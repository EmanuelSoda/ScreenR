# Fit the empirical null by central matching

Internal function: near its mode the log density of a normal
distribution is a parabola. The parabola is fitted to the log density of
x within 1.5 median absolute deviations of the mode, where the hits are
rare, and gives the mean and the standard deviation of the null (central
matching, Efron 2004). When the fit fails (the parabola does not open
downwards), the median and the median absolute deviation are used.

## Usage

``` r
fit_null_central(x, width = 1.5)
```

## Arguments

- x:

  The gene statistics.

- width:

  The half-width of the fitted region, in median absolute deviations.

## Value

A named vector with mean and sd.

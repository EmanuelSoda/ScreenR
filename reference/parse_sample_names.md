# Parse Sample Names

Internal function that derives the Day and the Treatment of each sample
from names of the form Day_Treatment_Replicate.

## Usage

``` r
parse_sample_names(samples)
```

## Arguments

- samples:

  The sample names.

## Value

A tibble with the columns Sample, Day and Treatment.

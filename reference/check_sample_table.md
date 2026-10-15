# Check the Sample Table

Internal function that finds and renames the columns of the sample table
and checks that it describes all the samples of the count table.

## Usage

``` r
check_sample_table(sample_table, sample_columns, samples)
```

## Arguments

- sample_table:

  The sample table.

- sample_columns:

  The named vector with the names of the Sample, Day and Treatment
  columns.

- samples:

  The sample names of the count table.

## Value

The checked sample table, in the order of the count table.

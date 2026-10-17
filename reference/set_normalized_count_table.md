# Set the normalized count table

Sets the normalization of a ScreenR object from a table of normalized
counts, for example counts normalized outside ScreenR. Usually the
normalization is set with
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md).
ScreenR uses one normalization for the plots and the edgeR/limma models,
expressed as one effective library size for each sample, so the table
must be the raw counts divided by one factor for each sample (times
1e6): the effective library sizes are computed from it and the method is
recorded as "custom". Any other table is an error. The table is matched
to the counts by barcode and sample; an empty table removes the
normalization.

## Usage

``` r
set_normalized_count_table(object, normalized_count_table)

# S4 method for class 'screenr_object'
set_normalized_count_table(object, normalized_count_table)
```

## Arguments

- object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- normalized_count_table:

  A table with a Barcode column and one column of normalized counts for
  each sample.

## Value

The ScreenR object with the new normalized count table.

## See also

[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md),
[`get_normalized_count_table`](https://emanuelsoda.github.io/ScreenR/reference/get_normalized_count_table.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
normalized_count_table <- get_normalized_count_table(object)
#> ScreenR normalized count table: 5320 rows, 15 columns
set_normalized_count_table(object, normalized_count_table)
#> class: screenr_object 
#> dim: 5320 14 
#> metadata(2): screenr_layout normalization
#> assays(1): counts
#> rownames(5320): 10208 10502 ... 13876 13995
#> rowData names(5): Barcode Gene Gene_ID Sequence Library
#> colnames(14): T1 T2 ... Time4_B Time4_c
#> colData names(5): Sample Day Treatment Group EffectiveLibSize
#> normalization: custom (effective library sizes in colData(object)$EffectiveLibSize)
```

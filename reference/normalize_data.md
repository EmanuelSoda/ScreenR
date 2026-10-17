# Normalize the counts

Sets the normalization of a ScreenR object, so that samples sequenced at
different depths can be compared. The raw counts are not changed: the
normalization is the effective library size of each sample, and the
normalized count of a barcode is its raw count divided by the effective
library size of its sample, times 1e6 (counts per million). Three
methods are available:

- "cpm" (default): the effective library size is the total number of
  reads of the sample;

- "tmm": the total number of reads multiplied by the trimmed mean of
  M-values (TMM) normalization factor of edgeR
  ([`normLibSizes`](https://rdrr.io/pkg/edgeR/man/calcNormFactors.html)).
  This is more robust when strong hits deplete a large part of the
  library;

- "control": the number of reads of the control barcodes (for example
  non-targeting shRNAs, or the sgRNAs of non-essential genes) of the
  sample. This assumes that the control barcodes are not affected by the
  treatment.

## Usage

``` r
normalize_data(
  screenR_Object,
  method = c("cpm", "tmm", "control"),
  control_barcodes = NULL
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- method:

  The normalization method: "cpm" (default), "tmm" or "control".

- control_barcodes:

  The identifiers of the control barcodes for the "control" method
  (required by that method, ignored by the others). Identifiers that are
  not in the object give a warning; if none is in the object, it is an
  error.

## Value

The normalized ScreenR object.

## Details

The normalization is used consistently by the whole analysis: the
normalized counts
([`get_normalized_count_table`](https://emanuelsoda.github.io/ScreenR/reference/get_normalized_count_table.md)),
the data table
([`get_data_table`](https://emanuelsoda.github.io/ScreenR/reference/get_data_table.md))
and everything computed from it
([`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md),
[`plot_trend`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md),
[`plot_boxplot`](https://emanuelsoda.github.io/ScreenR/reference/plot_boxplot.md),
[`compute_slope`](https://emanuelsoda.github.io/ScreenR/reference/compute_slope.md),
...), the sample plots
([`plot_mds`](https://emanuelsoda.github.io/ScreenR/reference/plot_mds.md),
[`plot_pca`](https://emanuelsoda.github.io/ScreenR/reference/plot_pca.md),
[`plot_replicate_correlation`](https://emanuelsoda.github.io/ScreenR/reference/plot_replicate_correlation.md))
and the edgeR and limma models
([`create_edger_obj`](https://emanuelsoda.github.io/ScreenR/reference/create_edger_obj.md),
used by
[`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
[`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
[`compute_barcode_statistics`](https://emanuelsoda.github.io/ScreenR/reference/compute_barcode_statistics.md),
[`find_time_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md),
[`filter_by_variance`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_variance.md)
and
[`plot_barcode_hit`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md)),
whose offsets are the logarithms of the effective library sizes. The
models always use the raw counts. Logarithms and pseudocounts used by
the plots (for example log2(normalized + 1) in
[`plot_pca`](https://emanuelsoda.github.io/ScreenR/reference/plot_pca.md))
are display transformations: they do not change the normalization.

It is stored in the object: the effective library sizes in the
EffectiveLibSize column of the colData, the method and its provenance in
the metadata (see
[`get_normalization`](https://emanuelsoda.github.io/ScreenR/reference/get_normalization.md)).
Replacing the counts removes it; subsetting the object keeps the
effective library sizes of the retained samples (call normalize_data()
again to normalize on the retained barcodes only). Calling
normalize_data() again replaces the normalization, and every view of the
object follows immediately.

Every sample must have reads (and, for "control", reads of the control
barcodes): a sample with an effective library size of zero cannot be
normalized and is an error.

**Choosing the method.** In a dropout screen many barcodes are depleted,
so with "cpm" all the other barcodes look enriched. "tmm" reduces this
bias, but it assumes that most barcodes do not change and that the
changes go in both directions: when many barcodes change in the same
direction it corrects only part of the bias (in a simulation with 20% of
the genes depleted 8 times it removed about half of it). "control"
removes it when control barcodes known to be without effect are
available (for example the sgRNAs of non-essential genes).

## See also

[`get_normalization`](https://emanuelsoda.github.io/ScreenR/reference/get_normalization.md),
[`create_edger_obj`](https://emanuelsoda.github.io/ScreenR/reference/create_edger_obj.md),
[`get_normalized_count_table`](https://emanuelsoda.github.io/ScreenR/reference/get_normalized_count_table.md),
[`remove_all_zero_row`](https://emanuelsoda.github.io/ScreenR/reference/remove_all_zero_row.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
object <- normalize_data(object)
get_normalization(object)$method
#> [1] "cpm"
get_normalized_count_table(object)
#> ScreenR normalized count table: 5320 rows, 15 columns
#> # A tibble: 5,320 × 15
#>    Barcode    T1    T2 Time3_TRT_A Time3_TRT_B Time3_TRT_C Time3_A Time3_B
#>    <chr>   <dbl> <dbl>       <dbl>       <dbl>       <dbl>   <dbl>   <dbl>
#>  1 10208   330.  437.        365.        439.        311.    358.    425. 
#>  2 10502    26.2  34.6        20.5        59.5        16.7    49.3    17.4
#>  3 10796   141.  156.        164.        124.        132.    120.    121. 
#>  4 11089   356.  424.        336.        424.        375.    388.    346. 
#>  5 11382    67.2  65.7        82.6        36.9        26.3    58.4    65.4
#>  6 11675   687.  775.        722.        682.        669.    820.    797. 
#>  7 11969   455.  548.        623.        540.        558.    488.    438. 
#>  8 12263   144.  119.        112.        138.         95.0   158.    128. 
#>  9 12555   406.  413.        459.        518.        483.    408.    425. 
#> 10 12847    43.7  42.6        30.2        34.9        29.6    34.6    20.5
#> # ℹ 5,310 more rows
#> # ℹ 7 more variables: Time3_C <dbl>, Time4_TRT_A <dbl>, Time4_TRT_B <dbl>,
#> #   Time4_TRT_C <dbl>, Time4_A <dbl>, Time4_B <dbl>, Time4_c <dbl>

# TMM normalization: the same effective library sizes are used by the
# plots and by the edgeR models
object <- normalize_data(object, method = "tmm")
get_normalization(object)$effective_library_size
#>          T1          T2 Time3_TRT_A Time3_TRT_B Time3_TRT_C     Time3_A 
#>    10853404     9827013     6840595     9229604     9737088     9647737 
#>     Time3_B     Time3_C Time4_TRT_A Time4_TRT_B Time4_TRT_C     Time4_A 
#>     9179789     9641856     8799318     9717098    12044656    10302865 
#>     Time4_B     Time4_c 
#>     9128760     9182589 
create_edger_obj(object)$samples
#>               group lib.size norm.factors
#> T1            T1/T2 10853404            1
#> T2            T1/T2  9827013            1
#> Time3_TRT_A Treated  6840595            1
#> Time3_TRT_B Treated  9229604            1
#> Time3_TRT_C Treated  9737088            1
#> Time3_A     Control  9647737            1
#> Time3_B     Control  9179789            1
#> Time3_C     Control  9641856            1
#> Time4_TRT_A Treated  8799318            1
#> Time4_TRT_B Treated  9717098            1
#> Time4_TRT_C Treated 12044656            1
#> Time4_A     Control 10302865            1
#> Time4_B     Control  9128760            1
#> Time4_c     Control  9182589            1
```

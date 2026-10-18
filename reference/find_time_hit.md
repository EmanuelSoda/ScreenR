# Find hits with a time-course model

Finds the genes whose depletion (or enrichment) caused by the treatment
grows over time, using a single model for the whole time course instead
of a separate analysis for each time point. Both the treated and the
untreated samples must cover at least two time points.

## Usage

``` r
find_time_hit(
  screenR_Object,
  treatment,
  control,
  days,
  baseline = NULL,
  number_barcode = 3,
  direction = c("Down", "Up"),
  fdr = 0.05,
  fdr_method = c("BH", "storey"),
  time = NULL
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md),
  normalized with
  [`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md).
  The model uses its raw counts with its effective library sizes as
  offsets, the same normalization as the plots and the data table (see
  [`create_edger_obj`](https://emanuelsoda.github.io/ScreenR/reference/create_edger_obj.md)).

- treatment:

  The value(s) of the Treatment column of
  [`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md)
  for the treated samples.

- control:

  The value(s) of the Treatment column of
  [`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md)
  for the control samples.

- days:

  The time points to use, in temporal order, as in the Day column of
  [`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md).

- baseline:

  Optional value(s) of the Treatment column of samples taken before the
  treatment (for example the first time points), which are used as
  untreated samples.

- number_barcode:

  Only genes with more than number_barcode barcodes are tested (default
  3, so a gene needs at least 4 barcodes). The barcodes of a gene are
  tested together as a set, so this is not a number of barcodes that
  must change.

- direction:

  "Down" (default) for genes depleted over time in the treated samples
  or "Up" for enriched genes.

- fdr:

  The FDR threshold: only genes with an FDR lower than or equal to this
  value are returned (default 0.05). Use fdr = 1 to return all the genes
  with the given direction.

- fdr_method:

  How the FDR is computed: "BH" (Benjamini-Hochberg, the default) or
  "storey" (Benjamini-Hochberg multiplied by the estimated proportion of
  genes without effect).

- time:

  An optional named numeric vector with the real time of each time point
  of days (and of the days of the baseline samples), named by the values
  of the Day column, for example c(Time3 = 7, Time4 = 14). By default
  (NULL) the time points are numbered 0, 1, 2, ... in the order of days.

## Value

A tibble with one row for each hit in the requested direction and the
columns of [`camera`](https://rdrr.io/pkg/limma/man/camera.html) (Gene,
NGenes, Direction, PValue, FDR), where PValue is the one-sided p-value
for the requested direction and FDR is computed from it, plus Log2FC:
the median over the barcodes of the gene of c, the change of the
treatment log2 fold change per time point (or per unit of time when time
is given).

## Details

The barcode counts are modelled with edgeR as log(abundance) = a + b \*
time + d \* treated + c \* time \* treated, where time is the time of
the sample and treated is 1 for the treated samples and 0 for the
control and baseline samples. By default the time is the index of the
time point (0 for the first element of days, 1 for the second, ...), so
the time points are treated as equally spaced; give their real times
with the time argument (for example days after selection) to fit the
model against the actual time. The untreated samples and the treated
samples each follow their own line; the coefficient c is the difference
between the slopes, i.e. how much the treatment effect changes per time
point. A constant treatment effect gives c = 0. The barcodes of each
gene are then tested together with
[`camera`](https://rdrr.io/pkg/limma/man/camera.html), as in
[`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md).

With real times, c (and the Log2FC column) is the change of the
treatment log2 fold change per unit of time. With two time points the
model is saturated: c is the difference between the treatment effects at
the two time points divided by the time between them, so the p-values do
not depend on the times. With three or more time points that are not
equally spaced, the times matter: the model assumes that the treatment
effect changes linearly with time, and real times fit that line against
the actual time instead of the order of the samples. A treatment effect
that does not change linearly (for example one that reaches a plateau)
is not described well by this model.

## See also

[`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
[`summarise_hits`](https://emanuelsoda.github.io/ScreenR/reference/summarise_hits.md),
[`plot_barcode_trend`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_trend.md),
[`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
get_sample_table(object)
#> # A tibble: 14 × 3
#>    Sample      Day   Treatment
#>    <chr>       <chr> <chr>    
#>  1 T1          T1    T1       
#>  2 T2          T2    T2       
#>  3 Time3_TRT_A Time3 TRT      
#>  4 Time3_TRT_B Time3 TRT      
#>  5 Time3_TRT_C Time3 TRT      
#>  6 Time3_A     Time3 Time3    
#>  7 Time3_B     Time3 Time3    
#>  8 Time3_C     Time3 Time3    
#>  9 Time4_TRT_A Time4 TRT      
#> 10 Time4_TRT_B Time4 TRT      
#> 11 Time4_TRT_C Time4 TRT      
#> 12 Time4_A     Time4 Time4    
#> 13 Time4_B     Time4 Time4    
#> 14 Time4_c     Time4 Time4    
find_time_hit(object,
    treatment = "TRT", control = c("Time3", "Time4"),
    days = c("Time3", "Time4"), fdr = 1
)
#> Warning: 3rows with all zero counts
#> Warning: Zero sample variances detected, have been offset away from zero
#> # A tibble: 258 × 6
#>    Gene     NGenes Direction   PValue   FDR Log2FC
#>    <chr>     <dbl> <fct>        <dbl> <dbl>  <dbl>
#>  1 Gene_296     10 Down      0.000661 0.336 -0.676
#>  2 Gene_372     12 Down      0.00166  0.336 -0.729
#>  3 Gene_422     10 Down      0.00189  0.336 -0.473
#>  4 Gene_87      10 Down      0.00568  0.510 -0.412
#>  5 Gene_412     10 Down      0.00572  0.510 -0.569
#>  6 Gene_419     10 Down      0.00573  0.510 -0.582
#>  7 Gene_349     10 Down      0.00705  0.538 -0.270
#>  8 Gene_292     10 Down      0.00929  0.544 -0.327
#>  9 Gene_370     10 Down      0.00973  0.544 -0.282
#> 10 Gene_325     10 Down      0.0102   0.544 -0.231
#> # ℹ 248 more rows

# With the real times (days after selection): with two time points the
# p-values are the same, and Log2FC is per day
find_time_hit(object,
    treatment = "TRT", control = c("Time3", "Time4"),
    days = c("Time3", "Time4"), time = c(Time3 = 7, Time4 = 14), fdr = 1
)
#> Warning: 3rows with all zero counts
#> Warning: Zero sample variances detected, have been offset away from zero
#> # A tibble: 258 × 6
#>    Gene     NGenes Direction   PValue   FDR  Log2FC
#>    <chr>     <dbl> <fct>        <dbl> <dbl>   <dbl>
#>  1 Gene_296     10 Down      0.000661 0.336 -0.0965
#>  2 Gene_372     12 Down      0.00166  0.336 -0.104 
#>  3 Gene_422     10 Down      0.00189  0.336 -0.0675
#>  4 Gene_87      10 Down      0.00568  0.510 -0.0588
#>  5 Gene_412     10 Down      0.00572  0.510 -0.0813
#>  6 Gene_419     10 Down      0.00573  0.510 -0.0831
#>  7 Gene_349     10 Down      0.00705  0.538 -0.0386
#>  8 Gene_292     10 Down      0.00929  0.544 -0.0468
#>  9 Gene_370     10 Down      0.00973  0.544 -0.0404
#> 10 Gene_325     10 Down      0.0102   0.544 -0.0330
#> # ℹ 248 more rows
```

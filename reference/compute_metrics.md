# Compute the barcode metrics

Computes, for each barcode, the log2 fold change between the treated and
the control samples of a given time point, and its Z-score and robust
Z-score. The result is the input of the Z-score methods
([`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
[`find_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md))
and of several plots. More information about the Z-score and other
metrics used in genetic screens can be found in this
[paper](https://pubmed.ncbi.nlm.nih.gov/21515799/).

## Usage

``` r
compute_metrics(screenR_Object, control, treatment, day, pseudocount = 0.5)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- control:

  The value(s) of the Treatment column of the data table (see
  [`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md))
  that identify the control samples.

- treatment:

  The value(s) of the Treatment column of the data table that identify
  the treated samples.

- day:

  The day (time point) to use, as in the Day column of the data table.

- pseudocount:

  A small value (in counts per million) added to the treated and control
  mean frequencies before computing the log2 fold change, so that
  barcodes with zero reads get a finite fold change: Log2FC =
  log2((Treated + pseudocount) / (Control + pseudocount)). Default 0.5.
  Barcodes with zero reads in both treated and control are removed. The
  value is on the scale of the normalized count table, which is counts
  per million of all the reads for the "cpm" and "tmm" methods of
  [`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md),
  but per million reads of the control barcodes for the "control"
  method.

## Value

A tibble with one row for each barcode and the columns Barcode, Gene,
Treated and Control (the mean normalized counts), Log2FC, Zscore,
ZscoreRobust, Day and Treatment.

## Details

The function uses the data table, so run
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
and
[`compute_data_table`](https://emanuelsoda.github.io/ScreenR/reference/compute_data_table.md)
first. For each barcode the normalized counts of the treated and of the
control samples of the given day are averaged, then Log2FC =
log2((Treated + pseudocount) / (Control + pseudocount)), Zscore =
(Log2FC - mean(Log2FC)) / sd(Log2FC) and ZscoreRobust = (Log2FC -
median(Log2FC)) / mad(Log2FC), where the mean, standard deviation,
median and median absolute deviation are computed over all the barcodes.
Since the replicates are averaged, their variability is not used: see
[`compute_barcode_statistics`](https://emanuelsoda.github.io/ScreenR/reference/compute_barcode_statistics.md)
for a replicate-aware alternative.

## See also

[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
[`compute_barcode_statistics`](https://emanuelsoda.github.io/ScreenR/reference/compute_barcode_statistics.md),
[`find_discordant_barcodes`](https://emanuelsoda.github.io/ScreenR/reference/find_discordant_barcodes.md),
[`plot_zscore_distribution`](https://emanuelsoda.github.io/ScreenR/reference/plot_zscore_distribution.md),
[`plot_barcode_trend`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_trend.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
metrics <- compute_metrics(object,
    control = "Time3",
    treatment = "TRT", day = "Time3"
)
head(metrics)
#> # A tibble: 6 × 9
#>   Barcode Gene     Treated Control  Log2FC Zscore ZscoreRobust Day   Treatment
#>   <fct>   <chr>      <dbl>   <dbl>   <dbl>  <dbl>        <dbl> <chr> <chr>    
#> 1 1       Gene_386   296.    326.  -0.140  -0.394       -0.549 Time3 TRT      
#> 2 10      Gene_519   115.    110.   0.0618  0.202        0.273 Time3 TRT      
#> 3 100     Gene_84     30.8    16.9  0.849   2.53         3.48  Time3 TRT      
#> 4 1000    Gene_350   130.    127.   0.0343  0.120        0.161 Time3 TRT      
#> 5 10001   Gene_379   463.    492.  -0.0894 -0.245       -0.343 Time3 TRT      
#> 6 10002   Gene_379   169.    147.   0.201   0.612        0.839 Time3 TRT      
```

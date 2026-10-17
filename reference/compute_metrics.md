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
  value is on the scale of the normalized counts: per million reads of
  the effective library size of
  [`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
  (all the reads for "cpm", all the reads times the TMM factor for
  "tmm", the reads of the control barcodes for "control"). It is a
  display transformation that keeps the fold changes finite; it does not
  change the normalization.

## Value

A tibble with one row for each barcode and the columns Barcode, Gene,
Treated and Control (the mean normalized counts), Log2FC, Zscore,
ZscoreRobust, Day and Treatment.

## Details

The function uses the normalized counts (see
[`get_data_table`](https://emanuelsoda.github.io/ScreenR/reference/get_data_table.md)),
so run
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
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
#>   Barcode Gene     Treated Control  Log2FC  Zscore ZscoreRobust Day   Treatment
#>   <fct>   <chr>      <dbl>   <dbl>   <dbl>   <dbl>        <dbl> <chr> <chr>    
#> 1 10208   Gene_380   372.    382.  -0.0390 -0.0962       -0.138 Time3 TRT      
#> 2 10502   Gene_380    32.2    27.9  0.205   0.625         0.857 Time3 TRT      
#> 3 10796   Gene_380   140.    123.   0.190   0.579         0.794 Time3 TRT      
#> 4 11089   Gene_380   378.    365.   0.0510  0.170         0.229 Time3 TRT      
#> 5 11382   Gene_380    48.6    66.9 -0.456  -1.33         -1.84  Time3 TRT      
#> 6 11675   Gene_380   691.    829.  -0.263  -0.759        -1.05  Time3 TRT      
```

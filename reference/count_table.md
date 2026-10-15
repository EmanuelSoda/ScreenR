# Example count table

Barcode counts of an example loss-of-function shRNA screen (a chemical
synthetic lethality screen), with one row for each barcode and one
column for each sample. The shRNAs of a pooled
[Cellecta](https://cellecta.com/) library were sequenced, the reads were
aligned to the reference library with
[bowtie2](http://bowtie-bio.sourceforge.net/bowtie2/index.smd) and
sorted with [samtools](https://github.com/samtools/samtools). The
barcodes of the shRNAs that knock down a gene needed to survive the
treatment are expected to have fewer reads in the treated samples than
in the controls.

## Usage

``` r
data(count_table)
```

## Format

A data frame with 5323 rows and 15 columns. Besides the 5320 barcodes of
[`annotation_table`](https://emanuelsoda.github.io/ScreenR/reference/annotation_table.md),
it contains three rows with the barcode "\*" (unmapped reads, all zero),
which should be removed before the edgeR based analyses.

- Barcode:

  The barcode identifier, unique for each shRNA. It links the count
  table to the annotation table. Since several shRNAs target each gene,
  each with its own efficacy, every shRNA is tracked with its own
  barcode.

- Time_1:

  The counts at the first time point, before the infection and the
  treatment.

- Time_2:

  The counts after the infection and the puromycin selection (Cellecta
  protocol).

- Time_3_TRT_rep1, Time_3_TRT_rep2, Time_3_TRT_rep3:

  The counts of the three replicates of the treated samples at the first
  time point after the selection (usually 7 days after the puromycin
  wash).

- Time_3_rep1, Time_3_rep2, Time_3_rep3:

  The counts of the three replicates of the control samples at the same
  time point.

- Time_4_TRT_rep1, Time_4_TRT_rep2, Time_4_TRT_rep3:

  The counts of the three replicates of the treated samples at the
  second time point (usually 14 days after the puromycin wash).

- Time_4_rep1, Time_4_rep2, Time_4_rep3:

  The counts of the three replicates of the control samples at the same
  time point.

## See also

[`annotation_table`](https://emanuelsoda.github.io/ScreenR/reference/annotation_table.md),
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Examples

``` r
data(count_table)
head(count_table)
#>   Barcode Time_1 Time_2 Time_3_TRT_rep1 Time_3_TRT_rep2 Time_3_TRT_rep3
#> 1   10208   3520   4283            2493            3998            3018
#> 2   10502    280    339             140             542             162
#> 3   10796   1507   1533            1120            1125            1286
#> 4   11089   3797   4151            2296            3857            3645
#> 5   11382    717    644             564             336             256
#> 6   11675   7330   7593            4927            6211            6504
#>   Time_3_rep1 Time_3_rep2 Time_3_rep3 Time_4_TRT_rep1 Time_4_TRT_rep2
#> 1        3393        3829        3474            2890            3776
#> 2         468         157         162             290             152
#> 3        1136        1095        1214             127             395
#> 4        3677        3124        3458            2321            3221
#> 5         554         590         735             319             106
#> 6        7781        7183        8339            6914            7858
#>   Time_4_TRT_rep3 Time_4_rep1 Time_4_rep2 Time_4_rep3
#> 1            5630        3500        3288        3640
#> 2             418         654         270         189
#> 3             625         452         378         842
#> 4            3461        3726        3104        2561
#> 5             401         127         403         666
#> 6           10189       10465        8601        9076
```

# Tutorial 1: Getting your data into ScreenR

## About this tutorial

This is the first of three tutorials that explain step by step how to
use ScreenR:

1.  **Getting your data into ScreenR** (this tutorial): which tables you
    need, how to create the ScreenR object and how to check that ScreenR
    understood your data correctly.
2.  **Quality control**: how to check that the screen worked before
    looking for hits.
3.  **Finding and validating hits**: how to compare treated and control
    samples, how to combine the statistical methods and how to check the
    candidate genes.

Everything in ScreenR starts from a **ScreenR object**, which keeps all
the tables of your experiment together. At the end of this tutorial you
will know how to build it from your own data.

``` r

library(ScreenR)
library(dplyr)
```

## What ScreenR needs

ScreenR analyses pooled screens with barcoded shRNA (or sgRNA)
libraries: each gene is targeted by several barcodes, the barcodes are
counted by sequencing in each sample, and a gene whose barcodes
disappear (or increase) in the treated samples is a candidate hit. You
need three pieces of information.

| What | Content | Required |
|----|----|----|
| **Count table** | One row per barcode, one column with the barcode identifier and one column per sample with the number of reads | Yes |
| **Annotation table** | One row per barcode, with the barcode identifier and the gene it targets | Yes |
| **Groups** | The experimental group of each sample (for example `Time3_TRT`), in the same order as the sample columns | Yes, for the statistical analysis |
| **Sample table** | The time point (Day) and the Treatment of each sample | Only when the sample names do not follow the `Day_Treatment_Replicate` convention (see below) |

### A minimal example

The smallest possible input: two genes with three barcodes each, two
control and two treated samples at one time point.

``` r

toy_counts <- data.frame(
    Barcode = paste0("bc", 1:6),
    Day3_CTRL_A = c(520, 610, 480, 300, 350, 410),
    Day3_CTRL_B = c(500, 590, 505, 320, 330, 395),
    Day3_TRT_A = c(130, 160, 120, 310, 345, 400),
    Day3_TRT_B = c(150, 140, 110, 290, 360, 420)
)
toy_counts
#>   Barcode Day3_CTRL_A Day3_CTRL_B Day3_TRT_A Day3_TRT_B
#> 1     bc1         520         500        130        150
#> 2     bc2         610         590        160        140
#> 3     bc3         480         505        120        110
#> 4     bc4         300         320        310        290
#> 5     bc5         350         330        345        360
#> 6     bc6         410         395        400        420

toy_annotation <- data.frame(
    Barcode = paste0("bc", 1:6),
    Gene = rep(c("GeneA", "GeneB"), each = 3)
)
toy_annotation
#>   Barcode  Gene
#> 1     bc1 GeneA
#> 2     bc2 GeneA
#> 3     bc3 GeneA
#> 4     bc4 GeneB
#> 5     bc5 GeneB
#> 6     bc6 GeneB
```

The groups say which samples are replicates of the same condition. They
must be in the **same order as the sample columns** of the count table.

``` r

toy_groups <- factor(c("Day3_CTRL", "Day3_CTRL", "Day3_TRT", "Day3_TRT"))
```

Now the object can be created:

``` r

toy <- create_screenr_object(
    table = toy_counts,
    annotation = toy_annotation,
    groups = toy_groups
)
```

## Telling ScreenR what each sample is

Several functions need to know the **time point** (Day) and the
**treatment** of each sample, for example to compare treated and control
samples at Day 3.

### The naming convention

By default ScreenR reads them from the sample names, which should have
the form

**`Day_Treatment_Replicate`**, for example `Day3_TRT_A`: time point
`Day3`, treatment `TRT`, replicate `A`.

Always check how the samples were interpreted with
[`get_sample_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md):

``` r

get_sample_table(toy)
#> # A tibble: 4 × 3
#>   Sample      Day   Treatment
#>   <chr>       <chr> <chr>    
#> 1 Day3_CTRL_A Day3  CTRL     
#> 2 Day3_CTRL_B Day3  CTRL     
#> 3 Day3_TRT_A  Day3  TRT      
#> 4 Day3_TRT_B  Day3  TRT
```

### When the names have another form: the sample table

If your samples are called, for example, `S1`, `S2`, … or `ctrl_rep1`,
the convention does not apply. ScreenR then gives a message, and you
should describe the samples with a **sample table**. It can have any
column names: tell ScreenR which column is which with `sample_columns`.

``` r

counts_other_names <- toy_counts
colnames(counts_other_names) <- c("Barcode", "S1", "S2", "S3", "S4")

my_samples <- data.frame(
    sample_id = c("S1", "S2", "S3", "S4"),
    time = "Day3",
    condition = c("CTRL", "CTRL", "TRT", "TRT")
)
my_samples
#>   sample_id time condition
#> 1        S1 Day3      CTRL
#> 2        S2 Day3      CTRL
#> 3        S3 Day3       TRT
#> 4        S4 Day3       TRT

toy2 <- create_screenr_object(
    table = counts_other_names,
    annotation = toy_annotation,
    groups = toy_groups,
    sample_table = my_samples,
    sample_columns = c(
        Sample = "sample_id", Day = "time", Treatment = "condition"
    )
)
get_sample_table(toy2)
#> # A tibble: 4 × 3
#>   Sample Day   Treatment
#>   <chr>  <chr> <chr>    
#> 1 S1     Day3  CTRL     
#> 2 S2     Day3  CTRL     
#> 3 S3     Day3  TRT      
#> 4 S4     Day3  TRT
```

The rows of the sample table can be in any order: ScreenR matches them
to the columns of the count table by name. Every sample of the count
table must be in the sample table.

## Using your own column names

Your tables do not need columns called `Barcode` and `Gene`. Give their
names with `barcode_column` and `gene_column`; ScreenR renames them
internally and remembers the original names.

``` r

my_counts <- toy_counts
colnames(my_counts)[1] <- "shRNA_id"

my_annotation <- data.frame(
    shRNA_id = paste0("bc", 1:6),
    symbol = rep(c("GeneA", "GeneB"), each = 3)
)

toy3 <- create_screenr_object(
    table = my_counts, annotation = my_annotation, groups = toy_groups,
    barcode_column = "shRNA_id", gene_column = "symbol"
)
get_column_names(toy3)
#>    Barcode       Gene 
#> "shRNA_id"   "symbol"
```

If you forget to give the name, the error tells you what to do:

``` r

create_screenr_object(
    table = my_counts, annotation = my_annotation, groups = toy_groups,
    barcode_column = "shRNA_id"
)
#> Error:
#> ! The annotation table has no column called 'Gene'. Its columns are: shRNA_id, symbol. Use gene_column to give the name of the column.
```

## Automatic checks

When the object is created ScreenR checks the input and either fixes
small problems (telling you what it did) or stops with a message
explaining what is wrong:

| Situation | What ScreenR does |
|----|----|
| Counts stored as text (e.g. `"520"`) | Converts them to numbers, with a message |
| Non-numeric columns in the count table (e.g. a gene column) | Removes them, with a message: gene information belongs in the annotation |
| `groups` with a different length than the number of samples | Error |
| Duplicated barcodes | Warning |
| Barcodes of the count table missing from the annotation (e.g. unmapped reads `*`) | Warning |
| Duplicated barcodes in the annotation | Warning; only the first row is kept |
| A column name that does not exist | Error suggesting the most similar column |

For example, a count table read from a file with the counts as text and
an extra gene column:

``` r

messy_counts <- toy_counts
messy_counts$Day3_CTRL_A <- as.character(messy_counts$Day3_CTRL_A)
messy_counts$Gene <- toy_annotation$Gene

clean <- create_screenr_object(
    table = messy_counts, annotation = toy_annotation, groups = toy_groups
)
#> Column 'Day3_CTRL_A' of the count table was stored as text and has been converted to numbers.
#> Column(s) 'Gene' of the count table are not numeric, so they are not samples, and have been removed. Put the information on the barcodes in the annotation table.
get_count_table(clean)
#> ScreenR count table: 6 rows, 5 columns
#> # A tibble: 6 × 5
#>   Barcode Day3_CTRL_A Day3_CTRL_B Day3_TRT_A Day3_TRT_B
#>   <chr>         <dbl>       <dbl>      <dbl>      <dbl>
#> 1 bc1             520         500        130        150
#> 2 bc2             610         590        160        140
#> 3 bc3             480         505        120        110
#> 4 bc4             300         320        310        290
#> 5 bc5             350         330        345        360
#> 6 bc6             410         395        400        420
```

And a `groups` vector that does not match the samples:

``` r

create_screenr_object(
    table = toy_counts, annotation = toy_annotation,
    groups = factor(c("CTRL", "TRT"))
)
#> Error:
#> ! groups has 2 values but the count table has 4 samples (Day3_CTRL_A, Day3_CTRL_B, Day3_TRT_A, Day3_TRT_B). groups must have one value for each sample.
```

## A real dataset

ScreenR includes the data of a real shRNA chemical lethality screen:
about 530 genes with 10 barcodes each, two early time points (`Time1`,
`Time2`) and treated (`TRT`) and control (`CTRL`) samples in triplicate
at two later time points (`Time3`, `Time4`).

``` r

data(count_table)
data(annotation_table)

dim(count_table)
#> [1] 5323   15
head(count_table[, 1:5])
#>   Barcode Time_1 Time_2 Time_3_TRT_rep1 Time_3_TRT_rep2
#> 1   10208   3520   4283            2493            3998
#> 2   10502    280    339             140             542
#> 3   10796   1507   1533            1120            1125
#> 4   11089   3797   4151            2296            3857
#> 5   11382    717    644             564             336
#> 6   11675   7330   7593            4927            6211
head(annotation_table)
#>       Gene Barcode Gene_ID Sequence Library
#> 1 Gene_380   10208      NA       NA      NA
#> 2 Gene_380   10502      NA       NA      NA
#> 3 Gene_380   10796      NA       NA      NA
#> 4 Gene_380   11089      NA       NA      NA
#> 5 Gene_380   11382      NA       NA      NA
#> 6 Gene_380   11675      NA       NA      NA
```

The original sample names do not follow the convention, so we rename the
columns. The rows with barcode `*` count the reads that did not match
any barcode: they are not barcodes and are removed.

``` r

counts <- count_table
colnames(counts) <- c(
    "Barcode", "Time1", "Time2", "Time3_TRT_A", "Time3_TRT_B", "Time3_TRT_C",
    "Time3_CTRL_A", "Time3_CTRL_B", "Time3_CTRL_C",
    "Time4_TRT_A", "Time4_TRT_B", "Time4_TRT_C",
    "Time4_CTRL_A", "Time4_CTRL_B", "Time4_CTRL_C"
)
counts <- filter(counts, Barcode != "*")
```

The groups: one value per sample, in the order of the columns. The two
early time points are grouped together as the starting point of the
screen.

``` r

groups <- factor(c(
    "T1/T2", "T1/T2",
    rep("Time3_TRT", 3), rep("Time3_CTRL", 3),
    rep("Time4_TRT", 3), rep("Time4_CTRL", 3)
))
```

Create the object and check the samples. `Time1` and `Time2` have no
treatment part in their name: ScreenR tells us, and uses the whole name
as both Day and Treatment, which is what we want for these early
samples.

``` r

object <- create_screenr_object(
    table = counts, annotation = annotation_table, groups = groups
)
#> The Day and the Treatment of each sample are taken from the sample names (Day_Treatment_Replicate). For 2 sample(s) (Time1, Time2) they cannot be separated and are both set to the first part of the name. If this is not intended, give them with the sample_table argument. Check them with get_sample_table().
get_sample_table(object)
#> # A tibble: 14 × 3
#>    Sample       Day   Treatment
#>    <chr>        <chr> <chr>    
#>  1 Time1        Time1 Time1    
#>  2 Time2        Time2 Time2    
#>  3 Time3_TRT_A  Time3 TRT      
#>  4 Time3_TRT_B  Time3 TRT      
#>  5 Time3_TRT_C  Time3 TRT      
#>  6 Time3_CTRL_A Time3 CTRL     
#>  7 Time3_CTRL_B Time3 CTRL     
#>  8 Time3_CTRL_C Time3 CTRL     
#>  9 Time4_TRT_A  Time4 TRT      
#> 10 Time4_TRT_B  Time4 TRT      
#> 11 Time4_TRT_C  Time4 TRT      
#> 12 Time4_CTRL_A Time4 CTRL     
#> 13 Time4_CTRL_B Time4 CTRL     
#> 14 Time4_CTRL_C Time4 CTRL
```

## Looking inside the object

Use the `get_` functions to read the tables of the object and the `set_`
functions to replace them. The tables are views of the object: the
object stores each piece of information once (see below), and the tables
are built from it when you ask for them.

| Function | Returns |
|----|----|
| [`get_count_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_count_table.md) | The raw counts |
| [`get_annotation_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_annotation_table.md) | The annotation |
| [`get_groups()`](https://emanuelsoda.github.io/ScreenR/reference/get_groups.md) | The groups |
| [`get_sample_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md) | Day and Treatment of each sample |
| [`get_column_names()`](https://emanuelsoda.github.io/ScreenR/reference/get_column_names.md) | The original column names |
| [`get_normalization()`](https://emanuelsoda.github.io/ScreenR/reference/get_normalization.md) | The normalization: method and effective library sizes |
| [`get_normalized_count_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_normalized_count_table.md) | The normalized counts (after [`normalize_data()`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)) |
| [`get_data_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_data_table.md) | The tidy table used by most functions (after [`normalize_data()`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)) |

``` r

head(get_count_table(object)[, 1:4])
#> ScreenR count table: 5320 rows, 15 columns
#> # A tibble: 6 × 4
#>   Barcode Time1 Time2 Time3_TRT_A
#>   <chr>   <int> <int>       <int>
#> 1 10208    3520  4283        2493
#> 2 10502     280   339         140
#> 3 10796    1507  1533        1120
#> 4 11089    3797  4151        2296
#> 5 11382     717   644         564
#> 6 11675    7330  7593        4927
get_groups(object)
#>  [1] T1/T2      T1/T2      Time3_TRT  Time3_TRT  Time3_TRT  Time3_CTRL
#>  [7] Time3_CTRL Time3_CTRL Time4_TRT  Time4_TRT  Time4_TRT  Time4_CTRL
#> [13] Time4_CTRL Time4_CTRL
#> Levels: T1/T2 Time3_CTRL Time3_TRT Time4_CTRL Time4_TRT
```

### The object is a SummarizedExperiment

The ScreenR object is also a
[SummarizedExperiment](https://bioconductor.org/packages/SummarizedExperiment),
the standard Bioconductor container for this kind of data, so it can be
used directly with the Bioconductor tools that accept one:

| Part | Content |
|----|----|
| `assay(object, "counts")` | The raw counts: one row per barcode, one column per sample. The row names are the barcode identifiers |
| `rowData(object)` | The annotation of each barcode (Gene, …) |
| `colData(object)` | Sample, Day, Treatment and Group of each sample and, after [`normalize_data()`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md), its effective library size (`EffectiveLibSize`) |
| `metadata(object)$normalization` | How the counts were normalized (method, control barcodes, …) |

These parts are the only storage of the object. The barcode identifiers
are always text and are the row names of the counts, never a column of
counts: numeric identifiers such as `10208` are stored as `"10208"`. The
normalized counts and the data table are not stored: they are computed
from these parts each time, so they cannot get out of date.

``` r

object
#> class: screenr_object 
#> dim: 5320 14 
#> metadata(1): screenr_layout
#> assays(1): counts
#> rownames(5320): 10208 10502 ... 13876 13995
#> rowData names(5): Barcode Gene Gene_ID Sequence Library
#> colnames(14): Time1 Time2 ... Time4_CTRL_B Time4_CTRL_C
#> colData names(4): Sample Day Treatment Group
#> normalization: none (run normalize_data())
head(SummarizedExperiment::rowData(object))
#> DataFrame with 6 rows and 5 columns
#>           Barcode        Gene   Gene_ID  Sequence   Library
#>       <character> <character> <logical> <logical> <logical>
#> 10208       10208    Gene_380        NA        NA        NA
#> 10502       10502    Gene_380        NA        NA        NA
#> 10796       10796    Gene_380        NA        NA        NA
#> 11089       11089    Gene_380        NA        NA        NA
#> 11382       11382    Gene_380        NA        NA        NA
#> 11675       11675    Gene_380        NA        NA        NA
SummarizedExperiment::colData(object)
#> DataFrame with 14 rows and 4 columns
#>                    Sample         Day   Treatment      Group
#>               <character> <character> <character>   <factor>
#> Time1               Time1       Time1       Time1  T1/T2    
#> Time2               Time2       Time2       Time2  T1/T2    
#> Time3_TRT_A   Time3_TRT_A       Time3         TRT  Time3_TRT
#> Time3_TRT_B   Time3_TRT_B       Time3         TRT  Time3_TRT
#> Time3_TRT_C   Time3_TRT_C       Time3         TRT  Time3_TRT
#> ...                   ...         ...         ...        ...
#> Time4_TRT_B   Time4_TRT_B       Time4         TRT Time4_TRT 
#> Time4_TRT_C   Time4_TRT_C       Time4         TRT Time4_TRT 
#> Time4_CTRL_A Time4_CTRL_A       Time4        CTRL Time4_CTRL
#> Time4_CTRL_B Time4_CTRL_B       Time4        CTRL Time4_CTRL
#> Time4_CTRL_C Time4_CTRL_C       Time4        CTRL Time4_CTRL
```

Like any SummarizedExperiment, the object can be subset by barcode
(rows) and by sample (columns), and all its parts stay consistent:

``` r

treated <- object[, object$Treatment == "TRT"]
treated
#> class: screenr_object 
#> dim: 5320 6 
#> metadata(1): screenr_layout
#> assays(1): counts
#> rownames(5320): 10208 10502 ... 13876 13995
#> rowData names(5): Barcode Gene Gene_ID Sequence Library
#> colnames(6): Time3_TRT_A Time3_TRT_B ... Time4_TRT_B Time4_TRT_C
#> colData names(4): Sample Day Treatment Group
#> normalization: none (run normalize_data())
```

### Keeping the object consistent

Because each piece of information is stored once, changes propagate by
themselves:

- **Changing the counts**
  ([`set_count_table()`](https://emanuelsoda.github.io/ScreenR/reference/set_count_table.md),
  or `assay(object, "counts") <- ...`) removes the normalization, which
  no longer describes the counts: run
  [`normalize_data()`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
  again.
- **Normalizing again** (for example with another method) changes at
  once the normalized counts, the data table, the plots and the
  edgeR/limma models.
- **Changing the annotation**
  ([`set_annotation_table()`](https://emanuelsoda.github.io/ScreenR/reference/set_annotation_table.md),
  or `rowData(object) <- ...`) changes the gene of each barcode in all
  the following analyses.
- **Subsetting or reordering** the object keeps the barcodes, their
  annotation and the samples aligned by identifier. A normalized object
  keeps the effective library sizes of the retained samples, computed on
  all the barcodes; to normalize on the retained barcodes, call
  [`normalize_data()`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
  again after subsetting.
- Inconsistent changes are refused: duplicated barcode identifiers, a
  `Barcode` column of the rowData different from the row names, or
  invalid effective library sizes give an error.

Objects saved with older versions of ScreenR are converted automatically
by all the functions;
[`update_screenr_object()`](https://emanuelsoda.github.io/ScreenR/reference/update_screenr_object.md)
converts them explicitly. Those versions did not record how the counts
were normalized, so the converted object is not normalized: run
[`normalize_data()`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
on it.

## Preparing the data for the analysis

Three steps prepare the data for all the following analyses.

**1. Remove the barcodes with zero reads in every sample.** They carry
no information.

``` r

object <- remove_all_zero_row(object)
```

**2. Normalize the counts.** Samples are sequenced at different depths,
so the counts are converted to counts per million (CPM). The default
divides by the total number of reads of each sample; `method = "tmm"`
and `method = "control"` are discussed in tutorial 2. The raw counts are
not changed: the normalization is the effective library size of each
sample, and the same normalization is used by the plots, the data table
and the statistical models.

``` r

object <- normalize_data(object)
get_normalization(object)$effective_library_size
#>        Time1        Time2  Time3_TRT_A  Time3_TRT_B  Time3_TRT_C Time3_CTRL_A 
#>     10671757      9799724      6824877      9107354      9717868      9485195 
#> Time3_CTRL_B Time3_CTRL_C  Time4_TRT_A  Time4_TRT_B  Time4_TRT_C Time4_CTRL_A 
#>      9016812      9568624      8933915      9883209     12325124     10351136 
#> Time4_CTRL_B Time4_CTRL_C 
#>      9180192      9315365
head(get_normalized_count_table(object)[, 1:4])
#> ScreenR normalized count table: 5317 rows, 15 columns
#> # A tibble: 6 × 4
#>   Barcode Time1 Time2 Time3_TRT_A
#>   <chr>   <dbl> <dbl>       <dbl>
#> 1 10208   330.  437.        365. 
#> 2 10502    26.2  34.6        20.5
#> 3 10796   141.  156.        164. 
#> 4 11089   356.  424.        336. 
#> 5 11382    67.2  65.7        82.6
#> 6 11675   687.  775.        722.
```

**3. Look at the data table.** This is a long (“tidy”) table with one
row per barcode and sample, and it is used by most of the plotting and
analysis functions. It is computed from the normalized object when
needed
([`compute_data_table()`](https://emanuelsoda.github.io/ScreenR/reference/compute_data_table.md),
needed by older versions of ScreenR, is no longer necessary).

``` r

data_table <- get_data_table(object)
#> ScreenR normalized data table: 74438 rows, 9 columns
head(select(data_table, Barcode, Gene, Sample, Frequency, Day, Treatment))
#> # A tibble: 6 × 6
#>   Barcode Gene     Sample       Frequency Day   Treatment
#>   <fct>   <chr>    <fct>            <dbl> <chr> <chr>    
#> 1 10208   Gene_380 Time1             330. Time1 Time1    
#> 2 10208   Gene_380 Time2             437. Time2 Time2    
#> 3 10208   Gene_380 Time3_TRT_A       365. Time3 TRT      
#> 4 10208   Gene_380 Time3_TRT_B       439. Time3 TRT      
#> 5 10208   Gene_380 Time3_TRT_C       311. Time3 TRT      
#> 6 10208   Gene_380 Time3_CTRL_A      358. Time3 CTRL
```

| Column                           | Meaning                                  |
|----------------------------------|------------------------------------------|
| `Barcode`, `Gene`                | The barcode and its gene                 |
| `Sample`                         | The sample                               |
| `Frequency`                      | The normalized count (CPM)               |
| `Day`, `Treatment`               | From the sample table                    |
| `Sequence`, `Library`, `Gene_ID` | Copied from the annotation, when present |

## Checklist

Before moving on to the quality control:

The count table has one row per barcode and one numeric column per
sample, plus the barcode column.

The annotation links every barcode to its gene (`barcode_column`,
`gene_column` if the names are different).

`groups` has one value per sample, in the order of the columns.

[`get_sample_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md)
shows the right Day and Treatment for every sample (otherwise use
`sample_table`).

You ran
[`remove_all_zero_row()`](https://emanuelsoda.github.io/ScreenR/reference/remove_all_zero_row.md)
and
[`normalize_data()`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md).

The next tutorial, **2. Quality control**, continues from this object.

``` r

sessionInfo()
#> R version 4.6.1 (2026-06-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.5 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] dplyr_1.2.1      ScreenR_1.15.16  BiocStyle_2.40.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] SummarizedExperiment_1.42.0 gtable_0.3.6               
#>  [3] xfun_0.62                   bslib_0.12.0               
#>  [5] ggplot2_4.0.3               Biobase_2.72.0             
#>  [7] lattice_0.22-9              vctrs_0.7.3                
#>  [9] tools_4.6.1                 generics_0.1.4             
#> [11] stats4_4.6.1                tibble_3.3.1               
#> [13] pkgconfig_2.0.3             Matrix_1.7-5               
#> [15] RColorBrewer_1.1-3          S7_0.2.2                   
#> [17] desc_1.4.3                  S4Vectors_0.50.3           
#> [19] lifecycle_1.0.5             compiler_4.6.1             
#> [21] farver_2.1.2                stringr_1.6.0              
#> [23] textshaping_1.0.5           statmod_1.5.2              
#> [25] Seqinfo_1.2.0               htmltools_0.5.9            
#> [27] sass_0.4.10                 yaml_2.3.12                
#> [29] pillar_1.11.1               pkgdown_2.2.1              
#> [31] jquerylib_0.1.4             tidyr_1.3.2                
#> [33] DelayedArray_0.38.2         cachem_1.1.0               
#> [35] limma_3.68.5                abind_1.4-8                
#> [37] tidyselect_1.2.1            locfit_1.5-9.12            
#> [39] digest_0.6.39               stringi_1.8.9              
#> [41] purrr_1.2.2                 bookdown_0.48              
#> [43] fastmap_1.2.0               grid_4.6.1                 
#> [45] cli_3.6.6                   SparseArray_1.12.3         
#> [47] magrittr_2.0.5              patchwork_1.3.2            
#> [49] S4Arrays_1.12.1             utf8_1.2.6                 
#> [51] withr_3.0.3                 edgeR_4.10.6               
#> [53] scales_1.4.0                rmarkdown_2.32             
#> [55] XVector_0.52.0              matrixStats_1.5.0          
#> [57] otel_0.2.0                  ragg_1.5.2                 
#> [59] evaluate_1.0.5              knitr_1.52                 
#> [61] GenomicRanges_1.64.0        IRanges_2.46.0             
#> [63] rlang_1.3.0                 glue_1.8.1                 
#> [65] BiocManager_1.30.27         BiocGenerics_0.58.1        
#> [67] jsonlite_2.0.0              R6_2.6.1                   
#> [69] MatrixGenerics_1.24.0       systemfonts_1.3.2          
#> [71] fs_2.1.0
```

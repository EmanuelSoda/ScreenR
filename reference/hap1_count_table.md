# HAP1 CRISPR-Cas9 knockout screen: count table

sgRNA read counts of a genome-wide CRISPR-Cas9 knockout screen in HAP1
cells with the TKOv3 library (Hart et al. 2017): the T0 sample and three
replicates after 3 and 18 days of growth. The sgRNAs of the genes needed
for growth (essential genes) are depleted over time. It is the example
dataset of the CRISPR tutorial.

## Usage

``` r
data(hap1_count_table)
```

## Format

A data frame with 13600 rows (sgRNAs) and 8 columns:

- Barcode:

  The sgRNA identifier (gene and guide sequence).

- T0:

  The counts at the start of the screen.

- T3_A, T3_B, T3_C:

  The counts of the three replicates after 3 days.

- T18_A, T18_B, T18_C:

  The counts after 18 days.

## Source

Hart T, Tong AHY, Chan K, et al. (2017). Evaluation and design of
genome-wide CRISPR/SpCas9 knockout screens. G3 7:2719-2727.
[doi:10.1534/g3.117.041277](https://doi.org/10.1534/g3.117.041277)

## Details

To keep the package small, the dataset is a subset of the library
(70,877 sgRNAs, 4 for each of about 18,000 genes): all the sgRNAs of the
core essential and non-essential reference genes
([`hart_reference_genes`](https://emanuelsoda.github.io/ScreenR/reference/hart_reference_genes.md)),
all the sgRNAs on chromosome 15, the sgRNAs of 1500 other randomly
chosen genes and the control sgRNAs (EGFP, LacZ, luciferase). Because it
is a subset, it must not be used with
[`correct_crispr_bias`](https://emanuelsoda.github.io/ScreenR/reference/correct_crispr_bias.md),
which needs the complete library. The script that builds it is in the
scripts folder of the installed package
(`system.file("scripts", package = "ScreenR")`).

The data come from the example data of BAGEL2
(<https://github.com/hart-lab/bagel>), distributed under the MIT
license, Copyright (c) 2020 Hart Lab (see
`system.file("LICENSES_data.txt", package = "ScreenR")`).

## See also

[`hap1_annotation_table`](https://emanuelsoda.github.io/ScreenR/reference/hap1_annotation_table.md),
[`hart_reference_genes`](https://emanuelsoda.github.io/ScreenR/reference/hart_reference_genes.md)

## Examples

``` r
data(hap1_count_table)
head(hap1_count_table)
#>                     Barcode  T0 T3_A T3_B T3_C T18_A T18_B T18_C
#> 1 EGFP_TGGTTGTCGGGCAGCAGCAC 537  222  150  216   243   115   304
#> 2 EGFP_GGTTGTCGGGCAGCAGCACG 210  197   92  125   195   177   115
#> 3 EGFP_TTCAAGTCCGCCATGCCCGA 517  423  255  282   329   309   433
#> 4 EGFP_CGGCGGTCACGAACTCCAGC 278  167   54  120   165   184    65
#> 5 EGFP_GGTGCCCATCCTGGTCGAGC  83   99   41   19   107   103    44
#> 6 EGFP_GAAGTTCGAGGGCGACACCC 268  145  153  204   133   295   228
```

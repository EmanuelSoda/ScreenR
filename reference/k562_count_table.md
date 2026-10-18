# K562 shRNA essentiality screen: count table

Read counts of a published genome-wide shRNA essentiality screen in K562
cells (Morgens et al. 2016), with reference essential and non-essential
genes
([`k562_reference_genes`](https://emanuelsoda.github.io/ScreenR/reference/k562_reference_genes.md)).
In an essentiality screen the cells are grown for some time after the
infection with the library: the barcodes of the genes needed for growth
(essential genes) are depleted. Since the essential genes are known, the
dataset can be used to evaluate how well a method finds the true hits.

## Usage

``` r
data(k562_count_table)
```

## Format

A data frame with 67358 rows (shRNAs) and 4 columns:

- Barcode:

  The shRNA identifier. It starts with the NCBI (Entrez) gene ID of the
  targeted gene (0 for the non-targeting controls).

- Plasmid:

  The counts in the plasmid library, before the screen.

- T14_A, T14_B:

  The counts of the two replicates after 14 days of growth.

## Source

Morgens DW, Deans RM, Li A, Bassik MC (2016). Systematic comparison of
CRISPR/Cas9 and RNAi screens for essential genes. Nat Biotechnol
34:634-636. [doi:10.1038/nbt.3567](https://doi.org/10.1038/nbt.3567)

## Details

To keep the package small, the dataset is a subset of the original
library (about 500,000 shRNAs): all the shRNAs of the 681 core essential
and 904 non-essential reference genes, of 1000 other randomly chosen
genes and 1000 randomly chosen non-targeting controls. The script that
builds the subset is in the scripts folder of the installed package
(`system.file("scripts", package = "ScreenR")`); the licenses of the
original data are in
`system.file("LICENSES_data.txt", package = "ScreenR")`.

The counts come from the test data of the casTLE software
(<https://bitbucket.org/dmorgens/castle>), distributed under the
BSD-2-Clause license, Copyright (c) 2016, David W Morgens.

## See also

[`k562_annotation_table`](https://emanuelsoda.github.io/ScreenR/reference/k562_annotation_table.md),
[`k562_reference_genes`](https://emanuelsoda.github.io/ScreenR/reference/k562_reference_genes.md)

## Examples

``` r
data(k562_count_table)
data(k562_annotation_table)
head(k562_count_table)
#>        Barcode Plasmid T14_A T14_B
#> 1 0_H01__01204      68    19    55
#> 2 0_H01__01217      69    53    75
#> 3 0_H01__01229      49     2     7
#> 4 0_H01__01247      62    77   124
#> 5 0_H01__01258      56   165   196
#> 6 0_H01__01278     141   105   180

# The sample names do not follow the Day_Treatment_Replicate convention,
# so the samples are described with a sample table: the plasmid and day 14
# are the time points, and the screen has no treatment
samples <- data.frame(
    Sample = c("Plasmid", "T14_A", "T14_B"),
    Day = c("Plasmid", "T14", "T14"),
    Treatment = "Untreated"
)
object <- create_screenr_object(
    table = k562_count_table, annotation = k562_annotation_table,
    groups = factor(c("Plasmid", "T14", "T14")),
    sample_table = samples
)
```

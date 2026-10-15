# K562 shRNA essentiality screen: reference essential and non-essential genes

The core essential genes (CEGv2) and the non-essential genes (NEGv1)
defined by Hart et al. from many genome-wide screens, restricted to the
genes of
[`k562_count_table`](https://emanuelsoda.github.io/ScreenR/reference/k562_count_table.md).
They are the standard reference to evaluate essentiality screens: the
core essential genes should be depleted and the non-essential genes not.

## Usage

``` r
data(k562_reference_genes)
```

## Format

A data frame with 1585 rows (genes) and 3 columns:

- Gene:

  The gene symbol.

- Gene_ID:

  The NCBI (Entrez) gene ID.

- Class:

  "core essential" (681 genes) or "non-essential" (904 genes).

## Source

Hart T, Brown KR, Sircoulomb F, Rottapel R, Moffat J (2014). Measuring
error rates in genomic perturbation screens: gold standards for human
functional genomics. Mol Syst Biol 10:733.
[doi:10.15252/msb.20145216](https://doi.org/10.15252/msb.20145216) .
Hart T, Tong AHY, Chan K, Van Leeuwen J, Seetharaman A, Aregger M, et
al. (2017). Evaluation and design of genome-wide CRISPR/SpCas9 knockout
screens. G3 7:2719-2727.
[doi:10.1534/g3.117.041277](https://doi.org/10.1534/g3.117.041277)

## Details

The lists come from the BAGEL repository
(<https://github.com/hart-lab/bagel>), distributed under the MIT
license, Copyright (c) 2020 Hart Lab.

## See also

[`k562_count_table`](https://emanuelsoda.github.io/ScreenR/reference/k562_count_table.md)

## Examples

``` r
data(k562_reference_genes)
table(k562_reference_genes$Class)
#> 
#> core essential  non-essential 
#>            681            904 
```

# Reference essential and non-essential human genes

The core essential genes (CEGv2, 684 genes) and the non-essential genes
(NEGv1, 926 genes) defined by Hart et al. from many genome-wide CRISPR
and shRNA screens. They are the standard reference to check the quality
of proliferation (dropout) screens in human cells with
[`compute_screen_quality`](https://emanuelsoda.github.io/ScreenR/reference/compute_screen_quality.md)
and to scale fold changes with
[`scale_by_reference`](https://emanuelsoda.github.io/ScreenR/reference/scale_by_reference.md):
the essential genes should be depleted and the non-essential genes not.
[`k562_reference_genes`](https://emanuelsoda.github.io/ScreenR/reference/k562_reference_genes.md)
is the subset of these genes present in the K562 dataset.

## Usage

``` r
data(hart_reference_genes)
```

## Format

A data frame with 1610 rows (genes) and 4 columns:

- Gene:

  The gene symbol.

- HGNC_ID:

  The HGNC identifier.

- Entrez_ID:

  The NCBI (Entrez) gene ID.

- Class:

  "core essential" or "non-essential".

## Source

Hart T, Brown KR, Sircoulomb F, Rottapel R, Moffat J (2014). Measuring
error rates in genomic perturbation screens: gold standards for human
functional genomics. Mol Syst Biol 10:733.
[doi:10.15252/msb.20145216](https://doi.org/10.15252/msb.20145216) .
Hart T, Tong AHY, Chan K, et al. (2017). Evaluation and design of
genome-wide CRISPR/SpCas9 knockout screens. G3 7:2719-2727.
[doi:10.1534/g3.117.041277](https://doi.org/10.1534/g3.117.041277)

## Details

The lists come from the BAGEL2 repository
(<https://github.com/hart-lab/bagel>), distributed under the MIT
license, Copyright (c) 2020 Hart Lab. The script that builds the dataset
is in the scripts folder of the installed package
(`system.file("scripts", package = "ScreenR")`).

## See also

[`compute_screen_quality`](https://emanuelsoda.github.io/ScreenR/reference/compute_screen_quality.md),
[`scale_by_reference`](https://emanuelsoda.github.io/ScreenR/reference/scale_by_reference.md),
[`k562_reference_genes`](https://emanuelsoda.github.io/ScreenR/reference/k562_reference_genes.md)

## Examples

``` r
data(hart_reference_genes)
table(hart_reference_genes$Class)
#> 
#> core essential  non-essential 
#>            684            926 
```

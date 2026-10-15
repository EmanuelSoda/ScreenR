# HAP1 CRISPR-Cas9 knockout screen: annotation table

Annotation of the sgRNAs of
[`hap1_count_table`](https://emanuelsoda.github.io/ScreenR/reference/hap1_count_table.md),
with their genomic position (TKOv3 library annotation distributed with
BAGEL2 for CRISPRcleanR).

## Usage

``` r
data(hap1_annotation_table)
```

## Format

A data frame with 13600 rows (sgRNAs) and 7 columns:

- Barcode:

  The sgRNA identifier, as in
  [`hap1_count_table`](https://emanuelsoda.github.io/ScreenR/reference/hap1_count_table.md).

- Gene:

  The symbol of the targeted gene (EGFP, LacZ and luciferase for the
  controls).

- Chromosome, Strand, Start, End:

  The position of the sgRNA target (NA for the controls).

- Class:

  "core essential" or "non-essential" for the reference genes (see
  [`hart_reference_genes`](https://emanuelsoda.github.io/ScreenR/reference/hart_reference_genes.md)),
  "control" for the controls and "other" for the other genes.

## Source

Hart T, Tong AHY, Chan K, et al. (2017). G3 7:2719-2727.
[doi:10.1534/g3.117.041277](https://doi.org/10.1534/g3.117.041277)

## See also

[`hap1_count_table`](https://emanuelsoda.github.io/ScreenR/reference/hap1_count_table.md)

## Examples

``` r
data(hap1_annotation_table)
table(hap1_annotation_table$Class)
#> 
#>        control core essential  non-essential          other 
#>            140           2524           3067           7869 
```

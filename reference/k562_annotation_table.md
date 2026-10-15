# K562 shRNA essentiality screen: annotation table

Annotation of the shRNAs of
[`k562_count_table`](https://emanuelsoda.github.io/ScreenR/reference/k562_count_table.md).

## Usage

``` r
data(k562_annotation_table)
```

## Format

A data frame with 67358 rows (shRNAs) and 4 columns:

- Barcode:

  The shRNA identifier, as in
  [`k562_count_table`](https://emanuelsoda.github.io/ScreenR/reference/k562_count_table.md).

- Gene:

  The symbol of the targeted gene ("non-targeting" for the controls).

- Gene_ID:

  The NCBI (Entrez) gene ID (NA for the controls).

- Class:

  "core essential" or "non-essential" for the reference genes (see
  [`k562_reference_genes`](https://emanuelsoda.github.io/ScreenR/reference/k562_reference_genes.md)),
  "other" for the other genes and "non-targeting" for the controls.

## Source

Morgens DW et al. (2016) Nat Biotechnol 34:634-636.
[doi:10.1038/nbt.3567](https://doi.org/10.1038/nbt.3567) . Gene symbols
from the NCBI gene_info file distributed with casTLE.

## See also

[`k562_count_table`](https://emanuelsoda.github.io/ScreenR/reference/k562_count_table.md)

## Examples

``` r
data(k562_annotation_table)
table(k562_annotation_table$Class)
#> 
#> core essential  non-essential  non-targeting          other 
#>          17648          22775           1000          25935 
```

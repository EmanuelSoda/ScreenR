# ScreenR: analysis of pooled shRNA and CRISPR-Cas9 screens

**ScreenR** finds candidate hits in loss-of-function pooled shRNA and
CRISPR-Cas9 screens, comparing a treatment with a control or the time
points of a dropout screen. It combines the statistical models of edgeR
and limma with the simplicity of the tidyverse, and provides plots for
each step of the analysis.

## Details

The analysis starts from a barcode count table and an annotation table,
which are stored in a ScreenR object
([`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)).
The counts are then normalized
([`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)),
checked with quality control plots, and the hits are found with several
statistical methods:

- the Z-score of the barcode log2 fold changes with a gene-level
  permutation test
  ([`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
  [Z-score](https://pubmed.ncbi.nlm.nih.gov/21515799/));

- [CAMERA](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3458527/)
  ([`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md));

- [ROAST](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2922896) and FRY
  ([`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md));

- a time-course model
  ([`find_time_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md))
  and a test for an effect at every time point
  ([`find_consistent_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_consistent_hit.md)).

CRISPR-Cas9 screens can be checked with essential and non-essential
reference genes
([`compute_screen_quality`](https://emanuelsoda.github.io/ScreenR/reference/compute_screen_quality.md))
and corrected for the copy-number bias with CRISPRcleanR
([`correct_crispr_bias`](https://emanuelsoda.github.io/ScreenR/reference/correct_crispr_bias.md)).
The results of the methods can be combined with
[`summarise_hits`](https://emanuelsoda.github.io/ScreenR/reference/summarise_hits.md)
and further filtered with the variance and slope filters
([`filter_by_variance`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_variance.md),
[`filter_by_slope`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md)).

## See also

Useful links:

- <https://emanuelsoda.github.io/ScreenR/>

- Report bugs at <https://github.com/EmanuelSoda/ScreenR/issues>

## Author

Emanuel Michele Soda <emanuelsoda@gmail.com>

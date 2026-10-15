# Correct CRISPR-Cas9 copy-number and proximity bias with CRISPRcleanR

In CRISPR-Cas9 screens, sgRNAs that cut in amplified regions of the
genome (or in clusters of nearby targets) deplete the cells whatever the
gene they target, producing false essential genes. This function
corrects the counts of a ScreenR object with the
[CRISPRcleanR](https://github.com/francescojm/CRISPRcleanR) package
(Iorio et al. 2018), which finds these regions by segmenting the sgRNA
fold changes along the genome, without needing copy-number data. ScreenR
does not implement the correction itself: it prepares the input, calls
the CRISPRcleanR functions and returns a new ScreenR object with the
corrected counts, ready for
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
and the hit-finding functions.

Use it for CRISPR-Cas9 knockout screens only (not for shRNA, CRISPRi or
CRISPRa screens), with a library annotation that gives the genomic
position of each sgRNA, and on the complete genome-wide library:
CRISPRcleanR estimates the bias from runs of neighbouring sgRNAs that
target different genes, assuming that most of them have no effect. On a
subset of genes enriched in hits (for example only the reference genes,
or the
[`hap1_count_table`](https://emanuelsoda.github.io/ScreenR/reference/hap1_count_table.md)
example), whole segments of essential genes look biased and their real
signal is removed.

## Usage

``` r
correct_crispr_bias(
  screenR_Object,
  library_annotation,
  control,
  treatment,
  min_reads = 30,
  min_targeted_genes = 3,
  label = "screen",
  outdir = tempfile("crisprcleanr_"),
  verbose = FALSE
)
```

## Arguments

- screenR_Object:

  A ScreenR object created with
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)
  from a CRISPR-Cas9 screen.

- library_annotation:

  The sgRNA library annotation in the format of CRISPRcleanR: a data
  frame with the sgRNA identifiers (the barcodes of the ScreenR object)
  as row names and at least the columns GENES, CHRM (chromosome),
  STARTpos and ENDpos. CRISPRcleanR includes the annotation of several
  libraries, for example
  `data(KY_Library_v1.0, package = "CRISPRcleanR")` or
  `Brunello_Library`, `AVANA_Library`, `GeCKO_Library_v2`.

- control:

  The name(s) of the control samples (for example the plasmid or T0), as
  in the columns of the count table.

- treatment:

  The names of the treated (or late time point) samples.

- min_reads:

  sgRNAs with fewer reads than this in the controls (mean) are removed
  (default 30, the CRISPRcleanR default).

- min_targeted_genes:

  Only segments with sgRNAs targeting at least this number of different
  genes are corrected (default 3, the CRISPRcleanR default).

- label:

  A name for the screen, used by CRISPRcleanR in its messages and output
  files.

- outdir:

  The directory where CRISPRcleanR writes its intermediate files (by
  default a temporary directory).

- verbose:

  Show the progress messages of CRISPRcleanR (default FALSE).

## Value

A ScreenR object with the corrected counts of the control and treated
samples. `S4Vectors::metadata(object)$crisprcleanr` is a list with the
uncorrected sgRNA log2 fold changes (logFCs), the corrected fold changes
(corrected_logFCs) and the segments (segments) returned by CRISPRcleanR.

## Details

CRISPRcleanR is not on CRAN or Bioconductor, so it is not installed with
ScreenR. Install it with
`BiocManager::install("francescojm/CRISPRcleanR")` (or
`remotes::install_github("francescojm/CRISPRcleanR")`).

The function runs the standard CRISPRcleanR workflow on the given
control and treated samples:

1.  `ccr.NormfoldChanges`: removes the sgRNAs with fewer than
    `min_reads` reads (mean of the controls), normalizes the counts and
    computes the sgRNA log2 fold changes;

2.  `ccr.logFCs2chromPos`: sorts the fold changes by genomic position;

3.  `ccr.GWclean`: segments the fold changes and corrects the segments
    with a biased response;

4.  `ccr.correctCounts`: computes corrected counts from the corrected
    fold changes.

The returned object contains only the control and treated samples and
the sgRNAs kept by CRISPRcleanR. Its counts are the corrected normalized
counts of CRISPRcleanR (rounded to integers, so that edgeR can model
them). The CRISPRcleanR results are in
`S4Vectors::metadata(object)$crisprcleanr`.

## References

Iorio F, Behan FM, Goncalves E, et al. (2018). Unsupervised correction
of gene-independent cell responses to CRISPR-Cas9 targeting. BMC
Genomics 19:604.
[doi:10.1186/s12864-018-4989-y](https://doi.org/10.1186/s12864-018-4989-y)

## See also

[`compute_screen_quality`](https://emanuelsoda.github.io/ScreenR/reference/compute_screen_quality.md)
to check the screen with reference genes before and after the
correction.

## Examples

``` r
# Needs the CRISPRcleanR package, which is not on CRAN or Bioconductor:
# BiocManager::install("francescojm/CRISPRcleanR")
if (crisprcleanr_available()) {
    ccr <- "CRISPRcleanR"
    # The KY library annotation distributed with CRISPRcleanR
    KY_Library_v1.0 <- get(utils::data("KY_Library_v1.0", package = ccr))
    file <- system.file("extdata", "HT-29_counts.tsv", package = ccr)
    counts <- read.delim(file)
    # A few chromosomes to keep the example fast
    keep <- KY_Library_v1.0[counts$sgRNA, "CHRM"] %in% c("8", "17")
    counts <- counts[keep, ]
    colnames(counts) <- c("Barcode", "Gene", "Plasmid", "R1", "R2", "R3")
    object <- create_screenr_object(
        table = counts[, -2],
        annotation = counts[, c("Barcode", "Gene")],
        groups = factor(c("Plasmid", "Late", "Late", "Late")),
        sample_table = data.frame(
            Sample = c("Plasmid", "R1", "R2", "R3"), Day = "Screen",
            Treatment = c("Plasmid", "Late", "Late", "Late")
        )
    )
    corrected <- correct_crispr_bias(object,
        library_annotation = KY_Library_v1.0,
        control = "Plasmid", treatment = c("R1", "R2", "R3")
    )
    corrected
}
```

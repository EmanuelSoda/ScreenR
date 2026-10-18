# Changelog

## ScreenR 1.15.16

- Tutorial 3 (finding and validating hits) uses the current functions: a
  sample table and palettes at creation,
  [`theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html),
  TMM normalization, FRY as the recommended test, the empirical null,
  [`plot_common_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_common_hit.md)
  with any number of methods (UpSet plot),
  [`find_consistent_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_consistent_hit.md),
  [`find_time_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md)
  with real times, and `plot_trend(color_by = )`,
  [`plot_treatment_separation()`](https://emanuelsoda.github.io/ScreenR/reference/plot_treatment_separation.md)
  and
  [`plot_barcode_heatmap()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_heatmap.md)
  to check the candidates.

- Tutorial 4 (CRISPR-Cas9 screens) uses palettes,
  [`theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html),
  the trend of the reference genes over the real days, their sgRNAs with
  [`plot_barcode_heatmap()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_heatmap.md),
  the overlap of the methods with
  [`plot_common_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_common_hit.md),
  and explains how the baseline of the normalization affects the
  self-contained tests.

- [`plot_treatment_separation()`](https://emanuelsoda.github.io/ScreenR/reference/plot_treatment_separation.md)
  plots descriptive treatment/control abundance ratios for any two
  condition labels and configurable time metadata. It retains all
  selected stages, supports real times and sample subsets, and inherits
  the user theme. Missing comparisons are never assigned zero effects.

- `plot_trend(color_by = "Treatment")` overlays separate condition
  trends with biological sample points and lines joining time-point
  means. The original ungrouped regression remains the default without
  color_by; `trend = "lm"` explicitly fits separate regressions. Object
  palettes are used, and `reference_groups` shows shared baselines once
  as neutral points. `connect_reference` joins the shared reference
  history to condition means; `highlight_difference` shades the observed
  gap between two conditions. No theme is applied by these new plots;
  use
  [`theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html)
  in user code.

- [`plot_barcode_heatmap()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_heatmap.md)
  shows individual barcode abundances across samples, with gene and
  sample-variable panels. Display log2 normalized counts,
  barcode-centered values or values relative to explicit reference
  samples.

- Model designs now carry sample identifiers and are checked against the
  object before fitting. Custom designs must have sample identifiers as
  row names in object order. Named contrast vectors and one-column
  contrast matrices are aligned to coefficient names; missing,
  duplicated and unknown names are rejected. Unnamed numeric contrast
  vectors remain positional.

- Package checks, coverage and website builds now run on pushes and pull
  requests to devel. Website builds on devel do not deploy GitHub Pages.

- [`find_consistent_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_consistent_hit.md)
  tests directional effects shared by all supplied contrasts using FRY,
  ROAST, CAMERA or gene-level Z-scores (random or empirical reference).
  [`combine_consistent_hits()`](https://emanuelsoda.github.io/ScreenR/reference/combine_consistent_hits.md)
  accepts complete one-sided gene-level p-value tables, combines their
  raw p-values with an intersection-union test and applies BH or BY
  correction across genes. Missing or duplicated genes and invalid
  p-values are errors.

- [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)
  and
  [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md)
  gain `keep_all = TRUE` to retain opposite observed directions with
  their requested one-sided p-values; defaults are unchanged. The
  longitudinal vignette uses the built-in consistency test.

- Palettes: `create_screenr_object(palettes = )` and the new
  [`set_palettes()`](https://emanuelsoda.github.io/ScreenR/reference/get_palettes.md)/[`get_palettes()`](https://emanuelsoda.github.io/ScreenR/reference/get_palettes.md)
  store colors for the values of the sample variables (Treatment, Group,
  Day, Sample, …), checked against the object and kept when it is
  subset. Every plot that colors the samples by one of these variables
  uses them:
  [`plot_mapped_reads()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mapped_reads.md),
  [`plot_mapped_reads_distribution()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mapped_reads_distribution.md),
  [`plot_barcode_lost()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_lost.md),
  [`plot_barcode_lost_for_gene()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_lost_for_gene.md),
  [`plot_gini()`](https://emanuelsoda.github.io/ScreenR/reference/plot_gini.md),
  [`plot_mds()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mds.md),
  [`plot_pca()`](https://emanuelsoda.github.io/ScreenR/reference/plot_pca.md)
  and
  [`plot_boxplot()`](https://emanuelsoda.github.io/ScreenR/reference/plot_boxplot.md).
  An explicit `palette` argument replaces the palette of the object.
  [`plot_mds()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mds.md)
  and
  [`plot_pca()`](https://emanuelsoda.github.io/ScreenR/reference/plot_pca.md)
  accept the name of a sample variable in `groups` (for example
  `groups = "Treatment"`).
  [`plot_zscore_distribution()`](https://emanuelsoda.github.io/ScreenR/reference/plot_zscore_distribution.md),
  which receives the tables of
  [`compute_metrics()`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
  rather than the object, has a new `palette` argument (for example
  `palette = get_palettes(object)$Day`).

- The longitudinal vignette sets palettes at the creation of the object
  and uses TMM normalization throughout.

- [`plot_common_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_common_hit.md)
  has been rewritten: it takes the results of any number of methods,
  from any hit-finding function of ScreenR
  ([`find_gene_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
  [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
  [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
  [`find_time_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md),
  [`find_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md),
  …) or vectors of gene names, as separate arguments or as one named
  list, with an optional `fdr` threshold for results computed with
  `fdr = 1`. Up to three methods give a Venn diagram, more an UpSet plot
  (`type` chooses). Both are drawn with ggplot2 and patchwork and apply
  no theme, so ScreenR no longer depends on ggvenn. Barcode-level tables
  are refused. Calls of previous versions keep working (unnamed results
  are now labelled with the expression given, except `hit_zscore`,
  `hit_camera` and `hit_roast`, which keep their labels).

- [`find_common_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_common_hit.md)
  takes the same input as
  [`plot_common_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_common_hit.md):
  any number of methods, with `common_in` (all the methods by default)
  and `fdr`.

- The longitudinal vignette compares five methods at each time point
  with UpSet plots, ranks the candidates with explicit rules (agreement
  of the self-contained tests, number of shRNAs, discordant shRNAs,
  dependence on one replicate, fitness, time trend) and ends with the
  hits to validate and their plots at T3 and T4. It sets the ggplot2
  theme with
  [`theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html).

- [`find_time_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md)
  has a new argument `time`, the real time of each time point (for
  example `time = c(Time3 = 7, Time4 = 14)`), validated as in
  [`compute_slope()`](https://emanuelsoda.github.io/ScreenR/reference/compute_slope.md)
  and
  [`plot_trend()`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md).
  The model is then fitted against the actual time and `Log2FC` is the
  change of the treatment effect per unit of time. By default
  (`time = NULL`) the time points are numbered 0, 1, 2, … as before, so
  existing results do not change; with two time points the p-values are
  the same with or without real times.

- [`compute_slope()`](https://emanuelsoda.github.io/ScreenR/reference/compute_slope.md),
  [`filter_by_slope()`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md)
  and
  [`plot_trend()`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md)
  reject times that are not finite.

- New vignette “Analysis of a longitudinal screen: from counts to
  treatment-specific hits”, a complete analysis of the example screen
  (T0, post-puromycin baseline, control and treated cultures at two time
  points with three replicates): quality control, one normalization for
  the whole analysis, one edgeR model with all the samples, treatment
  effects at each time point, the difference between depletion at both
  time points (intersection-union test), on average and changing over
  time
  ([`find_time_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md)),
  fitness genes shared by all the cultures, evidence at the level of the
  shRNAs, a sensitivity analysis and the current limitations of ScreenR
  for longitudinal designs.

### One source of truth for the data

- The ScreenR object stores each piece of information once, in its
  SummarizedExperiment parts: the raw counts in
  `assay(object, "counts")` (one row per barcode, one column per sample,
  the barcode identifiers as row names), the annotation of the barcodes
  in the rowData, the sample information, groups and per-sample
  replicate labels in the colData. The normalized counts and the data
  table are no longer stored: they are computed from these parts when
  needed, so they cannot get out of date.
  [`compute_data_table()`](https://emanuelsoda.github.io/ScreenR/reference/compute_data_table.md)
  only checks that the object is normalized and is no longer necessary.
- Barcode identifiers are always character and never become count
  columns: numeric identifiers are converted to their decimal form
  (10208 and “10208” are the same barcode, 1e5 is “100000”), in the
  count table, the annotation and the control barcodes.
- Stricter input checks in
  [`create_screenr_object()`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)
  and
  [`set_count_table()`](https://emanuelsoda.github.io/ScreenR/reference/set_count_table.md):
  missing identifiers, and missing, infinite or negative counts are
  errors; duplicated barcodes are an error unless
  `duplicate_barcodes = "sum"` or `"first"` (the example `count_table`
  has three “\*” rows: remove them, as the tutorials do); barcodes
  annotated more than once with different values are an error unless
  `duplicate_annotation = "first"` (exact copies are merged).
- The object is checked (validity method): unique identifiers, a Barcode
  column of the rowData equal to the row names, valid counts and a
  complete normalization. `assay<-`, `assays<-`, `rowData<-`,
  `colData<-` (and `$<-`) and `dimnames<-` keep these rules (see
  [`?"screenr_object-replace"`](https://emanuelsoda.github.io/ScreenR/reference/screenr_object-replace.md)).
- Changes propagate: replacing the counts with different values (with
  [`set_count_table()`](https://emanuelsoda.github.io/ScreenR/reference/set_count_table.md)
  or `assay<-`) removes the normalization; normalizing again changes
  every view at once; replacing the annotation
  ([`set_annotation_table()`](https://emanuelsoda.github.io/ScreenR/reference/set_annotation_table.md)
  or `rowData<-`) changes the gene of each barcode in all the following
  analyses; subsetting and reordering keep the barcodes, the annotation
  and the samples aligned by identifier.
- [`get_annotation_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_annotation_table.md)
  returns the annotation of the barcodes of the object (rowData)
  followed by the annotation rows of barcodes that are not in the count
  table, which are kept apart.
  [`barcodes_per_gene()`](https://emanuelsoda.github.io/ScreenR/reference/barcodes_per_gene.md)
  counts the barcodes of the object.
- [`set_data_table()`](https://emanuelsoda.github.io/ScreenR/reference/set_data_table.md)
  is deprecated: the data table cannot be replaced (it only accepts the
  current data table, with a warning).
  [`set_normalized_count_table()`](https://emanuelsoda.github.io/ScreenR/reference/set_normalized_count_table.md)
  only accepts tables that scale each sample by one factor, stored as
  effective library sizes (method “custom”).

### One normalization for the whole analysis

- [`normalize_data()`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
  no longer stores normalized counts: it records the effective library
  size of each sample in `colData(object)$EffectiveLibSize` and the
  method, the control barcodes and the provenance in
  `metadata(object)$normalization` (see the new
  [`get_normalization()`](https://emanuelsoda.github.io/ScreenR/reference/get_normalization.md)).
  The raw counts are never changed.
- The same normalization is used everywhere: the normalized counts, the
  data table and everything computed from it
  ([`compute_metrics()`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md),
  the plots,
  [`compute_slope()`](https://emanuelsoda.github.io/ScreenR/reference/compute_slope.md),
  …),
  [`plot_mds()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mds.md),
  [`plot_pca()`](https://emanuelsoda.github.io/ScreenR/reference/plot_pca.md),
  [`plot_explained_variance()`](https://emanuelsoda.github.io/ScreenR/reference/plot_explained_variance.md),
  [`plot_replicate_correlation()`](https://emanuelsoda.github.io/ScreenR/reference/plot_replicate_correlation.md)
  and the edgeR and limma models.
  [`create_edger_obj()`](https://emanuelsoda.github.io/ScreenR/reference/create_edger_obj.md),
  used by
  [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
  [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md)
  (ROAST and FRY),
  [`compute_barcode_statistics()`](https://emanuelsoda.github.io/ScreenR/reference/compute_barcode_statistics.md),
  [`find_time_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md),
  [`filter_by_variance()`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_variance.md)
  and
  [`plot_barcode_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md),
  builds the DGEList from the raw counts with the effective library
  sizes as library sizes (normalization factors 1), so the offsets of
  the models are the same normalization as the plots and TMM is applied
  once.
- Before, the edgeR/limma based methods always used the total counts of
  the object (CPM), whatever the method of
  [`normalize_data()`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md),
  and
  [`plot_replicate_correlation()`](https://emanuelsoda.github.io/ScreenR/reference/plot_replicate_correlation.md)
  used CPM too. **Results change** with `method = "tmm"` or `"control"`:
  in the HAP1 screen (day 18 vs T0) FRY calls 694 genes with TMM and 979
  with the non-essential sgRNAs as controls at 5% FDR, where it called
  721 with any method before. With the default `"cpm"` the results are
  unchanged, except when the object was subset after normalization: the
  models now use the library sizes of the normalization instead of the
  totals of the retained barcodes (small changes, at most 0.09 in log10
  p-value in a test on 300 genes).
- Operations that need normalized counts (data table, metrics, trend and
  sample plots, statistical methods) stop with an informative error when
  the object is not normalized, instead of using empty tables or
  normalizing silently
  ([`plot_mds()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mds.md),
  [`plot_pca()`](https://emanuelsoda.github.io/ScreenR/reference/plot_pca.md)
  and
  [`plot_explained_variance()`](https://emanuelsoda.github.io/ScreenR/reference/plot_explained_variance.md)
  used CPM silently). Raw-count quality control
  ([`compute_gini()`](https://emanuelsoda.github.io/ScreenR/reference/compute_gini.md),
  [`mapped_reads()`](https://emanuelsoda.github.io/ScreenR/reference/mapped_reads.md),
  [`barcode_lost()`](https://emanuelsoda.github.io/ScreenR/reference/barcode_lost.md),
  …) does not need normalization.
- Zero library sizes, and control barcodes that are missing (all of
  them) or have no reads in a sample, are errors in
  [`normalize_data()`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md);
  control barcodes partly missing give a warning.
- [`?normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
  explains that TMM corrects only part of the compositional bias when
  many barcodes change in the same direction (about half in a simulation
  with 20% of the genes depleted 8 times), while control barcodes remove
  it.

### Objects of older versions

- [`update_screenr_object()`](https://emanuelsoda.github.io/ScreenR/reference/update_screenr_object.md)
  (called by all the functions) converts objects created before 1.15.8
  and objects of versions 1.15.8 to 1.15.15. Their normalized counts and
  data table are not kept, because their normalization was not recorded:
  run
  [`normalize_data()`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
  on the converted object (a message says so). The internal example
  object has been converted and normalized with “cpm” (its normalized
  counts are identical to the old ones).

## ScreenR 1.15.15

- New tutorial “Tutorial 4: CRISPR-Cas9 screens”: installing
  CRISPRcleanR, describing a time-course dropout screen, choosing the
  sgRNA thresholds, checking the screen with reference genes, correcting
  copy-number bias with
  [`correct_crispr_bias()`](https://emanuelsoda.github.io/ScreenR/reference/correct_crispr_bias.md),
  finding the hits with the non-essential genes as reference, and
  positive-selection screens.
- New datasets `hap1_count_table` and `hap1_annotation_table`: a subset
  of the HAP1 TKOv3 CRISPR-Cas9 knockout screen of Hart et al. (2017),
  with the genomic position of each sgRNA.
- [`correct_crispr_bias()`](https://emanuelsoda.github.io/ScreenR/reference/correct_crispr_bias.md)
  documents that CRISPRcleanR must be run on the complete genome-wide
  library: on a subset of genes enriched in hits it removes real signal.
- Tutorial 4 adds FRY
  ([`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md))
  to the comparison of the methods (with a single T0 sample it works, as
  the variability is estimated from the replicates of the other time
  points), shows how conservative the methods without reference are on
  this subset enriched in essential genes, and explains the positive
  fold change of the non-essential genes after CPM normalization
  (normalizing to their sgRNAs puts them back at 0).
- [`find_discordant_barcodes()`](https://emanuelsoda.github.io/ScreenR/reference/find_discordant_barcodes.md)
  documents that it is for inspecting the hits, not for filtering the
  barcodes before finding them: the filter is circular and, in the K562
  screen, finds fewer essential and more non-essential genes.
- Tutorial 4 shows the general quality control plots on the HAP1 screen
  ([`plot_mapped_reads()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mapped_reads.md),
  [`plot_mapped_reads_distribution()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mapped_reads_distribution.md),
  [`plot_mds()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mds.md),
  [`plot_explained_variance()`](https://emanuelsoda.github.io/ScreenR/reference/plot_explained_variance.md)
  and
  [`plot_boxplot()`](https://emanuelsoda.github.io/ScreenR/reference/plot_boxplot.md)).
- [`plot_boxplot()`](https://emanuelsoda.github.io/ScreenR/reference/plot_boxplot.md)
  orders the fill legend as the samples (T0, T3, T18), not
  alphabetically (T0, T18, T3).
- [`plot_barcode_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md)
  is drawn with ggplot2 instead of
  [`limma::barcodeplot()`](https://rdrr.io/pkg/limma/man/barcodeplot.html)
  and returns a ggplot object (it returned NULL), so it can be saved,
  themed and combined with other plots. No theme is applied. It has the
  same enrichment line and barcode strip, the barcodes are colored by
  their log2 fold change, and a new panel (`show_log2fc`, TRUE by
  default) shows the log2 fold change of all the barcodes with those of
  the gene.
- [`plot_reference_separation()`](https://emanuelsoda.github.io/ScreenR/reference/plot_reference_separation.md)
  no longer applies
  [`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html):
  set the theme with
  [`ggplot2::theme_set()`](https://ggplot2.tidyverse.org/reference/get_theme.html)
  or by adding it to the plot.
- Clearer axis labels: “Explained variance” and “Principal component” in
  [`plot_explained_variance()`](https://emanuelsoda.github.io/ScreenR/reference/plot_explained_variance.md)
  (it said “Expressed Variance”), the dimensions of
  [`plot_mds()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mds.md)
  with the share of variance they explain, “Mapped reads” in
  [`plot_mapped_reads()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mapped_reads.md),
  “Reads per barcode” in
  [`plot_mapped_reads_distribution()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mapped_reads_distribution.md),
  “Lost barcodes” in
  [`plot_barcode_lost()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_lost.md)
  and
  [`plot_barcode_lost_for_gene()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_lost_for_gene.md),
  “Normalized counts” in
  [`plot_boxplot()`](https://emanuelsoda.github.io/ScreenR/reference/plot_boxplot.md)
  and “Mean normalized counts” in
  [`plot_trend()`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md)
  (they said “Frequency”).
- [`plot_trend()`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md)
  and
  [`plot_barcode_trend()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_trend.md)
  use `linewidth` instead of the `size` aesthetic deprecated in ggplot2
  3.4.0, so they no longer give a deprecation warning.
- New
  [`plot_pca()`](https://emanuelsoda.github.io/ScreenR/reference/plot_pca.md):
  the samples on two principal components (PC1 and PC2 by default,
  `components` selects others), with the share of the variance on the
  axes.
- [`plot_explained_variance()`](https://emanuelsoda.github.io/ScreenR/reference/plot_explained_variance.md)
  computed the principal components on the raw counts, so the first
  component mostly followed the sequencing depth of the samples
  (correlation 0.995 in the HAP1 screen). The principal components, and
  the MDS of
  [`plot_mds()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mds.md),
  are now computed on the log2 normalized counts, with the normalization
  chosen in
  [`normalize_data()`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
  (“cpm”, “tmm” or “control”; counts per million when the counts have
  not been normalized).
- [`plot_barcode_lost()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_lost.md)
  drew the bars twice, an opaque layer under the one with `alpha`, so
  `alpha` had no visible effect; the bars are drawn once.
- [`plot_trend()`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md)
  draws the samples at their time point instead of one position per
  sample, so replicates share the same time and the line is the one of
  [`compute_slope()`](https://emanuelsoda.github.io/ScreenR/reference/compute_slope.md)
  (used by
  [`filter_by_slope()`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md));
  before, the line was fitted along the order of the samples. New
  arguments of
  [`plot_trend()`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md),
  [`compute_slope()`](https://emanuelsoda.github.io/ScreenR/reference/compute_slope.md)
  and
  [`filter_by_slope()`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md):
  `time_column` (the column with the time points, “Day” by default) and
  `time` (the real times of the time points, for example
  `c(T0 = 0, T3 = 3, T18 = 18)`; by default they are equally spaced, as
  before).
- [`plot_trend()`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md)
  and
  [`plot_boxplot()`](https://emanuelsoda.github.io/ScreenR/reference/plot_boxplot.md)
  failed with several genes unless `nrow` or `ncol` was given; by
  default ggplot2 now chooses the layout.
- `find_gene_zscore_hit(reference = "empirical")`: the gene scores are
  compared with an empirical null, the distribution of the genes without
  effect estimated from the centre of the distribution of the gene
  statistics (`empirical_fit = "central"`, default, or `"one-sided"`).
  New
  [`plot_empirical_null()`](https://emanuelsoda.github.io/ScreenR/reference/plot_empirical_null.md)
  shows the fit. The random barcode sets of the default reference are
  conservative in dropout screens (the hits widen the null) and ignore
  that the barcodes of a gene vary together, which gives too small
  p-values in shRNA screens. In the genome-wide HAP1 CRISPR screen the
  empirical null found 90% (central) and 94% (one-sided) of the core
  essential genes at 5% FDR, against 58% with the default reference; in
  the K562 shRNA screen it kept the share of non-essential genes with a
  p-value below 0.05 under 4%, where the random barcodes reached 13%
  (default, with genome-wide proportions of essential genes) and 20%
  (non-essential genes as `control_genes`). See
  `benchmark/realdata/empirical_null.R`.
- New vignette “Choosing the reference of the gene-level test”, which
  suggests `reference = "empirical"` and shows why on the HAP1 and K562
  screens. The default of
  [`find_gene_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md)
  is still `reference = "random"`, so existing results do not change.

## ScreenR 1.15.14

### CRISPR screens

ScreenR works with CRISPR-Cas9 screens (tested on the HAP1 TKOv3 screen
of Hart et al. 2017, see the benchmark folder of the repository). New
functions make it easier:

- [`barcodes_per_gene()`](https://emanuelsoda.github.io/ScreenR/reference/barcodes_per_gene.md)
  counts the barcodes (shRNAs or sgRNAs) of each gene.
  [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
  [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
  [`find_time_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md),
  [`plot_barcode_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md),
  [`find_gene_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md),
  [`find_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md)
  and
  [`find_robust_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_robust_zscore_hit.md)
  now warn when their threshold on the number of barcodes per gene
  (`number_barcode` or `min_barcodes`) excludes more than 20% of the
  genes, and suggest a value that fits the library (for example with 4
  sgRNAs per gene). The defaults and the results do not change.
- [`compute_screen_quality()`](https://emanuelsoda.github.io/ScreenR/reference/compute_screen_quality.md)
  measures how well a screen separates essential from non-essential
  reference genes (NNMD, Cohen’s d, ROC AUC);
  [`plot_reference_separation()`](https://emanuelsoda.github.io/ScreenR/reference/plot_reference_separation.md)
  plots it;
  [`scale_by_reference()`](https://emanuelsoda.github.io/ScreenR/reference/scale_by_reference.md)
  scales the fold changes so that non-essential genes are at 0 and
  essential genes at -1. New dataset `hart_reference_genes`: the CEGv2
  core essential and NEGv1 non-essential genes of Hart et al.
- [`correct_crispr_bias()`](https://emanuelsoda.github.io/ScreenR/reference/correct_crispr_bias.md)
  corrects copy-number and proximity bias of CRISPR-Cas9 screens with
  the CRISPRcleanR package (Iorio et al. 2018), which must be installed
  separately (it is not on CRAN or Bioconductor:
  `BiocManager::install("francescojm/CRISPRcleanR")`); ScreenR only
  wraps it.
  [`crisprcleanr_available()`](https://emanuelsoda.github.io/ScreenR/reference/crisprcleanr_available.md)
  tells whether it is installed.

### Documentation

- The vignette “Benchmark against MAGeCK and other tools” adds BAGEL2,
  gscreend and two plain edgeR/limma workflows to the comparison, a
  CRISPR-Cas9 screen (HAP1, TKOv3) and the quality check of the K562
  screen with the reference genes. Its claims are toned down to match
  the results: ScreenR controls the FDR and ranks the hits as well as
  the best tools, but is more conservative at a fixed FDR.

### Other changes

- Removed the use of `.data` in tidyselect arguments of
  [`compute_data_table()`](https://emanuelsoda.github.io/ScreenR/reference/compute_data_table.md)
  and
  [`compute_metrics()`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
  (deprecated in tidyselect 1.2.0); results unchanged.

## ScreenR 1.15.13

- Enriched genes: every hit-finding method accepts `direction = "Up"`.
  [`find_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md)
  and
  [`find_robust_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_robust_zscore_hit.md)
  gain a `direction` argument (default `"Down"`, same results as
  before);
  [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)
  and
  [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md)
  now check `direction` and give an error for a value other than
  `"Down"` or `"Up"` (a typo such as `"up"` used to return no genes
  silently).
- New tests check that every method finds enriched and depleted genes
  without mixing them.

## ScreenR 1.15.12

### Documentation

- Every help page has been rewritten: clearer titles and descriptions,
  all arguments with their defaults, the columns of the returned tables,
  links between related functions and runnable examples. Several
  descriptions that did not match the code were corrected, for example
  `number_barcode` (genes need more than `number_barcode` barcodes to be
  tested), the return values of
  [`plot_common_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_common_hit.md),
  [`plot_mapped_reads_distribution()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mapped_reads_distribution.md),
  [`filter_by_variance()`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_variance.md)
  and
  [`filter_by_slope()`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md),
  and `group_var` in
  [`plot_trend()`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md),
  [`plot_boxplot()`](https://emanuelsoda.github.io/ScreenR/reference/plot_boxplot.md)
  and
  [`filter_by_slope()`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md).
- The `screenr_object` class has its own help page
  ([`?"screenr_object-class"`](https://emanuelsoda.github.io/ScreenR/reference/screenr_object-class.md)).
- New README and package website, with a quick start, a guide to the
  hit-finding methods and the benchmark against MAGeCK.

### Other changes

- [`compute_metrics()`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
  is about 40 times faster (same results).
- [`get_count_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_count_table.md),
  [`get_annotation_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_annotation_table.md),
  [`get_normalized_count_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_normalized_count_table.md)
  and
  [`get_data_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_data_table.md)
  report the size of the table with
  [`message()`](https://rdrr.io/r/base/message.html) instead of printing
  it, so it can be silenced with
  [`suppressMessages()`](https://rdrr.io/r/base/message.html).

## ScreenR 1.15.11

### Documentation

- New vignette “Benchmark against MAGeCK”: compares the ScreenR methods
  with MAGeCK RRA and MLE on simulated screens and on the K562 screen.
  MAGeCK and simulation results are precomputed in
  `inst/extdata/benchmark`; the ScreenR methods are run in the vignette.
  The scripts are in the `benchmark` folder of the GitHub repository.
- [`find_gene_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md):
  the documentation no longer calls control genes the best reference.
  Non-targeting shRNAs are often not a good model of a gene without
  effect (in the K562 screen they made hundreds of non-essential genes
  look depleted); check them before using `control_genes`.
- [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md):
  the documentation explains that ROAST and FRY need replicates in both
  groups and find almost no hits with a single control sample.

## ScreenR 1.15.10

### Calibrated p-values

A benchmark on simulated screens showed that the p-values of some
methods were too conservative (with no true hits, fewer than 5% of the
genes had p \< 0.05).

- [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
  [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md)
  and
  [`find_time_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md)
  now report one-sided p-values for the requested direction. camera and
  roast report two-sided p-values, which ScreenR used as they were: they
  were twice too large. The FDR is computed from the one-sided p-values
  over all the genes.
- [`find_gene_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md)
  is now a permutation test: the mean (or median, `statistic`) Z-score
  of the barcodes of each gene is compared with random sets of barcodes
  of the same size. The previous binomial test on the number of barcodes
  below a threshold had only a few possible p-values and was very
  conservative. The arguments `zscore_threshold` and the column
  `NBarcodesHit` are replaced by `statistic`, `n_permutations`, `seed`
  and the column `Score`.
- `find_gene_zscore_hit(control_genes = )` draws the random barcodes
  from control genes (for example non-targeting shRNAs), the best
  reference since it cannot contain true hits.
- `find_roast_hit(method = "fry")` uses
  [`limma::fry()`](https://rdrr.io/pkg/limma/man/roast.html), an exact
  and fast version of ROAST without the limit of 1 / (nrot + 1) on the
  p-values.
- `fdr_method = "storey"` in
  [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
  [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
  [`find_time_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md)
  and
  [`find_gene_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md)
  estimates the proportion of genes without effect and is less
  conservative than Benjamini-Hochberg when many genes have an effect.
- The description of ROAST is corrected: it is a self-contained test,
  not a competitive one.

## ScreenR 1.15.9

### New dataset

- `k562_count_table`, `k562_annotation_table` and
  `k562_reference_genes`: a subset of a published genome-wide shRNA
  essentiality screen in K562 cells (Morgens et al. 2016, Nat
  Biotechnol) with the reference core essential and non-essential genes
  of Hart et al. Since the true hits (the essential genes) are known,
  the dataset can be used to evaluate methods. The script that builds it
  is in `inst/scripts` and the licenses of the original data in
  `inst/LICENSES_data.txt`.

## ScreenR 1.15.8

### The ScreenR object is now a SummarizedExperiment

- The `screenr_object` class now extends `SummarizedExperiment`: the raw
  and normalized counts are the assays `counts` and `normalized`, the
  barcode annotation is in
  [`rowData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  and Sample, Day, Treatment and Group are in
  [`colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html).
  The object can be used with the Bioconductor tools that accept a
  `SummarizedExperiment` and subset like one
  (`object[barcodes, samples]`), keeping counts, normalized counts and
  data table consistent.
- All the ScreenR functions, including the `get_` and `set_` functions,
  return the same results as before: every exported function was run on
  the example data with the previous and the new class and gave
  identical outputs.
- Objects saved with older versions are converted automatically;
  [`update_screenr_object()`](https://emanuelsoda.github.io/ScreenR/reference/update_screenr_object.md)
  converts them explicitly.
- Code that reads the old slots directly needs to use the `get_`
  functions: `object@count_table` -\> `get_count_table(object)`,
  `object@normalized_count_table` -\>
  `get_normalized_count_table(object)`, `object@groups` -\>
  `get_groups(object)`. The slots `data_table` and `annotation_table`
  are unchanged.
- New dependencies: SummarizedExperiment and S4Vectors.

## ScreenR 1.15.7

### Documentation

- Three new step-by-step tutorials: “1. Getting your data into ScreenR”,
  “2. Quality control” and “3. Finding and validating hits”.

## ScreenR 1.15.6

### Bug fixes (found by an independent review of 1.15.4)

- [`find_time_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md)
  now includes a main effect for the treatment (log(abundance) = a + b
  \* time + d \* treated + c \* time \* treated), so that `c` measures
  how much the treatment effect changes over time. Previously the model
  assumed that treated and control samples start from the same level, so
  a constant treatment effect was reported as a growing one. The control
  and baseline samples must now cover at least two time points.
- The documentation of
  [`summarise_hits()`](https://emanuelsoda.github.io/ScreenR/reference/summarise_hits.md)
  and the vignette no longer claim that the list of genes with
  `MaxFDR <= alpha` keeps the FDR: `MaxFDR` is a per-gene level-alpha
  intersection-union test and a ranking score, but the combined list has
  no guaranteed false discovery rate.
- The examples of
  [`plot_barcode_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md)
  and
  [`filter_by_variance()`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_variance.md)
  (and some tests) combined `model.matrix(~ groups)` with
  `makeContrasts(Treated - Control)`, which subtracts the intercept;
  they now use
  [`create_design_matrix()`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md)
  and
  [`create_contrast()`](https://emanuelsoda.github.io/ScreenR/reference/create_contrast.md).
- Several examples called
  [`compute_metrics()`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
  with treatment and control swapped; they now use `treatment = "TRT"`.
- [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)
  and
  [`find_time_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md)
  no longer fail when only one gene can be tested, and give a clear
  error when no gene has more than `number_barcode` barcodes (also
  [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md)).
- `find_roast_hit(seed = )` restores the random number state of the
  session.
- [`compute_metrics()`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
  warns when the MAD of the log2 fold changes is 0;
  [`find_discordant_barcodes()`](https://emanuelsoda.github.io/ScreenR/reference/find_discordant_barcodes.md)
  reports the barcodes of genes with MAD 0 that differ from the median.
- [`find_time_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md)
  returns `Direction` as a factor, like the other methods.

### Changes not listed in 1.15.4

- [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
  [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
  [`plot_barcode_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md)
  and
  [`filter_by_variance()`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_variance.md)
  take the genes from the `Gene` column of the annotation instead of its
  first column, and check that the contrast matches the design matrix.
- The hit tables have new columns (`Log2FC`); code that selects their
  columns by position may need to be updated.

### Vignette

- The new vignette shows the overlap of the methods with
  [`plot_common_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_common_hit.md).

## ScreenR 1.15.5

### More general input

- [`create_screenr_object()`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)
  accepts tables with any column names: `barcode_column` and
  `gene_column` give the names of the barcode and gene columns, which
  are renamed internally to `Barcode` and `Gene`.
  [`get_column_names()`](https://emanuelsoda.github.io/ScreenR/reference/get_column_names.md)
  returns the original names. When a column is not found the error
  suggests the most similar column and the argument to use.
- The Day and Treatment of each sample can be given with `sample_table`
  (and `sample_columns` for its column names) instead of relying on
  sample names of the form `Day_Treatment_Replicate`.
  [`get_sample_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md)
  shows how the samples are interpreted; a message is given when sample
  names do not follow the convention.
- The input is checked when the object is created: `groups` must have
  one value for each sample; sample columns stored as text are converted
  to numbers; other non-numeric columns of the count table are removed
  with a message; duplicated barcodes and barcodes missing from the
  annotation give a warning; duplicated barcodes in the annotation are
  reduced to their first row.
- The annotation columns `Sequence`, `Library` and `Gene_ID` are now
  optional.
- New [`show()`](https://rdrr.io/r/methods/show.html) method, so that
  objects saved with older versions of ScreenR can still be printed.

### New features

- [`plot_gini()`](https://emanuelsoda.github.io/ScreenR/reference/plot_gini.md)
  plots the Gini index computed by
  [`compute_gini()`](https://emanuelsoda.github.io/ScreenR/reference/compute_gini.md).

## ScreenR 1.15.4

### Bug fixes in the vignette

- The design matrix of the example analysis was built with
  `model.matrix(~ groups)` and its columns renamed with
  `unique(groups)`, which does not match the column order: the contrast
  “Time3_TRT” actually compared the Time3 control samples with the
  baseline. The vignette now uses
  [`create_design_matrix()`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md)
  and
  [`create_contrast()`](https://emanuelsoda.github.io/ScreenR/reference/create_contrast.md)
  to compare treated and control samples of the same time point.
- The “at least 2 methods” common hits used `roast_hit_TRT$Day3` and
  `$Day6`, which do not exist, so ROAST was ignored.

### Changes that affect the results

- [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)
  gains `fdr = 0.05` and returns only the genes with FDR \<= fdr
  (previously all the genes with the requested direction). Use `fdr = 1`
  for the previous behaviour.
- [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md)
  gains `fdr = 0.05` and filters on the FDR; the raw p-value cut off
  `p_val` is now optional (default `NULL`, previously 0.05).
- [`compute_metrics()`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
  gains `pseudocount = 0.5`: the log2 fold change is
  `log2((Treated + pseudocount) / (Control + pseudocount))` instead of
  `log2(Treated / Control + 1e-7)`, and only barcodes with zero reads in
  both treated and control are removed (previously all barcodes with
  zero control reads).

### New features

- [`create_design_matrix()`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md)
  and
  [`create_contrast()`](https://emanuelsoda.github.io/ScreenR/reference/create_contrast.md)
  build the design matrix and the contrasts by group name.
  [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
  [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
  [`plot_barcode_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md)
  and
  [`filter_by_variance()`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_variance.md)
  accept a column name, a contrast vector or a
  [`limma::makeContrasts()`](https://rdrr.io/pkg/limma/man/makeContrasts.html)
  matrix.
- [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)
  and
  [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md)
  report `Log2FC`, the median log2 fold change of the barcodes of each
  gene;
  [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md)
  gains `seed`.
- [`normalize_data()`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
  gains `method = "tmm"` (edgeR TMM factors) and `method = "control"`
  (normalization to control barcodes).
- [`find_gene_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md):
  gene-level binomial test with FDR for the Z-score method.
- [`compute_barcode_statistics()`](https://emanuelsoda.github.io/ScreenR/reference/compute_barcode_statistics.md):
  replicate-aware barcode statistics with the edgeR quasi-likelihood
  F-test.
- [`find_time_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md):
  one model for the whole time course, testing whether the treatment
  effect changes over time.
- [`summarise_hits()`](https://emanuelsoda.github.io/ScreenR/reference/summarise_hits.md):
  combines the FDR of several methods (MaxFDR, intersection-union test).
- Quality control:
  [`plot_replicate_correlation()`](https://emanuelsoda.github.io/ScreenR/reference/plot_replicate_correlation.md),
  [`compute_gini()`](https://emanuelsoda.github.io/ScreenR/reference/compute_gini.md)
  and
  [`find_discordant_barcodes()`](https://emanuelsoda.github.io/ScreenR/reference/find_discordant_barcodes.md).
- New vignette “Improved statistical analysis with ScreenR”.
- edgeR \>= 4.0.0 is now required.

## ScreenR 1.15.3

### Changes

- [`find_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md)
  now checks the `metric` argument (`"median"`, the default, or
  `"mean"`) and gives a clear error for any other value.
- [`find_robust_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_robust_zscore_hit.md)
  now has a default `number_barcode = 6`, the same as
  [`find_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md),
  and returns an ungrouped tibble like the other hit functions.
- The `thresh` and `lfc` arguments of
  [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)
  are deprecated. They never had an effect on the result; passing them
  now gives a warning. Filter the returned table on its `FDR` column
  instead.
- Removed the stray `README.knit.md` file from the package.
- Added tests for these changes and for the 1.15.2 bug fixes.

## ScreenR 1.15.2

### Bug fixes (these change the results of some analyses)

- [`create_edger_obj()`](https://emanuelsoda.github.io/ScreenR/reference/create_edger_obj.md)
  now builds the edgeR object from the raw counts instead of the
  CPM-normalized counts, so edgeR uses the real library sizes. The
  annotation is matched to the counts by `Barcode`, and an error is
  raised when barcodes are missing from the annotation. This affects
  [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
  [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
  [`filter_by_variance()`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_variance.md)
  and
  [`plot_barcode_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md).
- The `number_barcode` argument of
  [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)
  and
  [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md)
  is now applied: only genes with more than `number_barcode` barcodes
  are tested. Previously every gene was tested.
- [`compute_metrics()`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
  now computes the standard robust Z-score,
  `(Log2FC - median(Log2FC)) / mad(Log2FC)`. This changes the
  ZscoreRobust values (and their plots); the hits of
  [`find_robust_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_robust_zscore_hit.md)
  do not change, since its rule only depends on the order of the scores.
- [`compute_slope()`](https://emanuelsoda.github.io/ScreenR/reference/compute_slope.md)
  (used by
  [`filter_by_slope()`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md))
  now regresses frequencies on the time points, with replicates sharing
  the same time point, instead of on the row number.
- [`plot_barcode_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md)
  now uses the design matrix to estimate the dispersion and honours its
  `number_barcode` argument.
- [`filter_by_slope()`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md)
  no longer prints the control slopes.

## ScreenR 1.15.1

- Fixed the vignette build failure with recent edgeR versions (`DPOTRF`
  error code -4 in
  [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)).

## ScreenR 0.99.53

- Changes in version 0.99.53:

- Update to the documentation for the example dataset as requested by
  the reviewer

- Pushed to Bioconductor

- Changes in version 0.99.52:

- Pushed to Bioconductor

- Changes in version 0.99.51:

- Pushed to Bioconductor

- Changes in version 0.99.5:

  - Now ScreenR uses R 4.2 or above
  - Lazy data is set to FALSE
  - In the DESCRIPTION is now present a BugReport link
  - Now all the function are imported using importFrom
  - The vignette has now the BiocStyle
  - All chunks of code of the vignette are set to show warnings
  - Implemented the set and get function for the ScreenR object

## ScreenR 0.99.41

- Changes in version 0.99.41:
  - Update to Bioconductor

## ScreenR 0.99.4

- Changes in version 0.99.4:
  - Changed dataframe for the example

## ScreenR 0.99.3

- Changes in version 0.99.3:
  - Fixed some other bug
  - Changed find_common_hit

## ScreenR 0.99.2

- Changes in version 0.99.2:
  - Fixed some other bug

## ScreenR 0.99.1

- Changes in version 0.99.1:
  - Fixed some bug

## ScreenR 0.99.0

- Changes in version 0.99.0:
  - Submitted to Bioconductor

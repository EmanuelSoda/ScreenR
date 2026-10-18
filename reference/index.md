# Package index

## Create the ScreenR object

Build the object from a count table and a barcode annotation, and update
objects saved with older versions of ScreenR.

- [`create_screenr_object()`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)
  : Create a ScreenR object
- [`screenr_object-class`](https://emanuelsoda.github.io/ScreenR/reference/screenr_object-class.md)
  [`screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/screenr_object-class.md)
  : The ScreenR object class
- [`` `assay<-`( ``*`<screenr_object>`*`,`*`<missing>`*`)`](https://emanuelsoda.github.io/ScreenR/reference/screenr_object-replace.md)
  [`` `assay<-`( ``*`<screenr_object>`*`,`*`<character>`*`)`](https://emanuelsoda.github.io/ScreenR/reference/screenr_object-replace.md)
  [`` `assay<-`( ``*`<screenr_object>`*`,`*`<numeric>`*`)`](https://emanuelsoda.github.io/ScreenR/reference/screenr_object-replace.md)
  [`` `assays<-`( ``*`<screenr_object>`*`,`*`<list>`*`)`](https://emanuelsoda.github.io/ScreenR/reference/screenr_object-replace.md)
  [`` `assays<-`( ``*`<screenr_object>`*`,`*`<SimpleList>`*`)`](https://emanuelsoda.github.io/ScreenR/reference/screenr_object-replace.md)
  [`` `rowData<-`( ``*`<screenr_object>`*`)`](https://emanuelsoda.github.io/ScreenR/reference/screenr_object-replace.md)
  [`` `colData<-`( ``*`<screenr_object>`*`,`*`<DataFrame>`*`)`](https://emanuelsoda.github.io/ScreenR/reference/screenr_object-replace.md)
  [`` `dimnames<-`( ``*`<screenr_object>`*`,`*`<list>`*`)`](https://emanuelsoda.github.io/ScreenR/reference/screenr_object-replace.md)
  : Replacement Methods of a ScreenR Object
- [`update_screenr_object()`](https://emanuelsoda.github.io/ScreenR/reference/update_screenr_object.md)
  : Update a ScreenR object
- [`remove_all_zero_row()`](https://emanuelsoda.github.io/ScreenR/reference/remove_all_zero_row.md)
  : Remove the barcodes with zero counts in all samples
- [`show(`*`<screenr_object>`*`)`](https://emanuelsoda.github.io/ScreenR/reference/show-screenr_object.md)
  : Show a ScreenR object
- [`` `[`( ``*`<screenr_object>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](https://emanuelsoda.github.io/ScreenR/reference/subset-screenr_object.md)
  : Subset a ScreenR object

## Access and modify the object

Getter and setter functions for the content of the ScreenR object.

- [`get_annotation_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_annotation_table.md)
  : Get the annotation table
- [`get_column_names()`](https://emanuelsoda.github.io/ScreenR/reference/get_column_names.md)
  : Get the original column names
- [`get_count_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_count_table.md)
  : Get the count table
- [`get_data_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_data_table.md)
  : Get the data table
- [`get_groups()`](https://emanuelsoda.github.io/ScreenR/reference/get_groups.md)
  : Get the groups
- [`get_normalization()`](https://emanuelsoda.github.io/ScreenR/reference/get_normalization.md)
  : Get the normalization
- [`get_normalized_count_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_normalized_count_table.md)
  : Get the normalized count table
- [`get_palettes()`](https://emanuelsoda.github.io/ScreenR/reference/get_palettes.md)
  [`set_palettes()`](https://emanuelsoda.github.io/ScreenR/reference/get_palettes.md)
  : Get or set the palettes of a ScreenR object
- [`get_replicates()`](https://emanuelsoda.github.io/ScreenR/reference/get_replicates.md)
  : Get the replicates
- [`get_sample_table()`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md)
  : Get the sample table
- [`set_annotation_table()`](https://emanuelsoda.github.io/ScreenR/reference/set_annotation_table.md)
  : Set the annotation table
- [`set_count_table()`](https://emanuelsoda.github.io/ScreenR/reference/set_count_table.md)
  : Set the count table
- [`set_data_table()`](https://emanuelsoda.github.io/ScreenR/reference/set_data_table.md)
  : Set the data table
- [`set_groups()`](https://emanuelsoda.github.io/ScreenR/reference/set_groups.md)
  : Set the groups
- [`set_normalized_count_table()`](https://emanuelsoda.github.io/ScreenR/reference/set_normalized_count_table.md)
  : Set the normalized count table
- [`set_replicates()`](https://emanuelsoda.github.io/ScreenR/reference/set_replicates.md)
  : Set the replicates

## Quality control

Check the sequencing depth, the library representation and the
replicates.

- [`count_mapped_reads()`](https://emanuelsoda.github.io/ScreenR/reference/count_mapped_reads.md)
  : Get the mapped reads of each barcode in each sample
- [`mapped_reads()`](https://emanuelsoda.github.io/ScreenR/reference/mapped_reads.md)
  : Compute the number of mapped reads of each sample
- [`plot_mapped_reads()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mapped_reads.md)
  : Plot the number of mapped reads of each sample
- [`plot_mapped_reads_distribution()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mapped_reads_distribution.md)
  : Plot the distribution of the mapped reads
- [`barcode_lost()`](https://emanuelsoda.github.io/ScreenR/reference/barcode_lost.md)
  : Compute the number of barcodes lost in each sample
- [`plot_barcode_lost()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_lost.md)
  : Plot the number of barcodes lost in each sample
- [`plot_barcode_lost_for_gene()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_lost_for_gene.md)
  : Plot the number of barcodes lost for each gene
- [`compute_gini()`](https://emanuelsoda.github.io/ScreenR/reference/compute_gini.md)
  : Compute the Gini index of each sample
- [`plot_gini()`](https://emanuelsoda.github.io/ScreenR/reference/plot_gini.md)
  : Plot the Gini index of each sample
- [`plot_replicate_correlation()`](https://emanuelsoda.github.io/ScreenR/reference/plot_replicate_correlation.md)
  : Plot the correlation between samples
- [`plot_mds()`](https://emanuelsoda.github.io/ScreenR/reference/plot_mds.md)
  : Plot a multidimensional scaling of the samples
- [`plot_pca()`](https://emanuelsoda.github.io/ScreenR/reference/plot_pca.md)
  : Plot a principal component analysis of the samples
- [`compute_explained_variance()`](https://emanuelsoda.github.io/ScreenR/reference/compute_explained_variance.md)
  : Compute the variance explained by the principal components
- [`plot_explained_variance()`](https://emanuelsoda.github.io/ScreenR/reference/plot_explained_variance.md)
  : Plot the variance explained by the principal components
- [`plot_boxplot()`](https://emanuelsoda.github.io/ScreenR/reference/plot_boxplot.md)
  : Plot the normalized counts of genes in each sample

## Normalization and barcode statistics

Normalize the counts and compute barcode-level fold changes and scores.

- [`normalize_data()`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
  : Normalize the counts
- [`compute_data_table()`](https://emanuelsoda.github.io/ScreenR/reference/compute_data_table.md)
  : Compute the data table
- [`compute_metrics()`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
  : Compute the barcode metrics
- [`compute_barcode_statistics()`](https://emanuelsoda.github.io/ScreenR/reference/compute_barcode_statistics.md)
  : Compute replicate-aware barcode statistics
- [`create_design_matrix()`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md)
  : Create the design matrix
- [`create_contrast()`](https://emanuelsoda.github.io/ScreenR/reference/create_contrast.md)
  : Create a contrast
- [`create_edger_obj()`](https://emanuelsoda.github.io/ScreenR/reference/create_edger_obj.md)
  : Create an edgeR object

## Find hits

Gene-level tests that return a p-value and a false discovery rate for
every gene.

- [`find_gene_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md)
  : Find hits with a gene-level Z-score test
- [`plot_empirical_null()`](https://emanuelsoda.github.io/ScreenR/reference/plot_empirical_null.md)
  : Plot the empirical null of the gene statistics
- [`find_camera_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)
  : Find hits with CAMERA
- [`find_roast_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md)
  : Find hits with ROAST or FRY
- [`find_consistent_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_consistent_hit.md)
  : Find genes with a consistent directional effect across contrasts
- [`combine_consistent_hits()`](https://emanuelsoda.github.io/ScreenR/reference/combine_consistent_hits.md)
  : Combine evidence for an effect in every contrast
- [`find_time_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md)
  : Find hits with a time-course model

## Combine and validate hits

Compare the methods and check the hits at the barcode level.

- [`summarise_hits()`](https://emanuelsoda.github.io/ScreenR/reference/summarise_hits.md)
  : Summarise the hits of several methods
- [`find_common_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_common_hit.md)
  : Find the hits in common between methods
- [`plot_common_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_common_hit.md)
  : Plot the hits in common between methods
- [`find_discordant_barcodes()`](https://emanuelsoda.github.io/ScreenR/reference/find_discordant_barcodes.md)
  : Find discordant barcodes
- [`plot_barcode_hit()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md)
  : Plot the barcodes of a gene (barcode plot)
- [`plot_barcode_trend()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_trend.md)
  : Plot the trend of the barcodes of genes over time
- [`plot_trend()`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md)
  : Plot the trend of genes over time
- [`plot_treatment_separation()`](https://emanuelsoda.github.io/ScreenR/reference/plot_treatment_separation.md)
  : Plot the separation between two screening conditions
- [`plot_barcode_heatmap()`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_heatmap.md)
  : Plot barcode abundance across samples
- [`plot_zscore_distribution()`](https://emanuelsoda.github.io/ScreenR/reference/plot_zscore_distribution.md)
  : Plot the distribution of the barcode log2 fold changes
- [`filter_by_slope()`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md)
  : Filter hits by the slope of their trend
- [`filter_by_variance()`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_variance.md)
  : Filter hits by the variance of their barcodes
- [`compute_slope()`](https://emanuelsoda.github.io/ScreenR/reference/compute_slope.md)
  : Compute the slope of the trend of genes

## CRISPR screens and reference genes

Choose the barcode thresholds for the library, check the screen with
essential and non-essential reference genes, and correct CRISPR-Cas9
copy-number bias with CRISPRcleanR.

- [`barcodes_per_gene()`](https://emanuelsoda.github.io/ScreenR/reference/barcodes_per_gene.md)
  : Count the barcodes of each gene
- [`compute_screen_quality()`](https://emanuelsoda.github.io/ScreenR/reference/compute_screen_quality.md)
  : Compute the quality of a screen from reference genes
- [`plot_reference_separation()`](https://emanuelsoda.github.io/ScreenR/reference/plot_reference_separation.md)
  : Plot the separation of reference genes
- [`scale_by_reference()`](https://emanuelsoda.github.io/ScreenR/reference/scale_by_reference.md)
  : Scale scores to reference genes
- [`correct_crispr_bias()`](https://emanuelsoda.github.io/ScreenR/reference/correct_crispr_bias.md)
  : Correct CRISPR-Cas9 copy-number and proximity bias with CRISPRcleanR
- [`crisprcleanr_available()`](https://emanuelsoda.github.io/ScreenR/reference/crisprcleanr_available.md)
  : Is CRISPRcleanR Available
- [`hart_reference_genes`](https://emanuelsoda.github.io/ScreenR/reference/hart_reference_genes.md)
  : Reference essential and non-essential human genes

## Legacy hit rules

Rules of earlier versions of ScreenR, kept for compatibility. They do
not control the false discovery rate; use the tests above to call hits.

- [`find_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md)
  : Find hits with the Z-score threshold rule
- [`find_robust_zscore_hit()`](https://emanuelsoda.github.io/ScreenR/reference/find_robust_zscore_hit.md)
  : Find hits with the robust Z-score threshold rule

## Datasets

Example screen, a published K562 shRNA screen and a HAP1 CRISPR-Cas9
screen with known essential genes.

- [`count_table`](https://emanuelsoda.github.io/ScreenR/reference/count_table.md)
  : Example count table
- [`annotation_table`](https://emanuelsoda.github.io/ScreenR/reference/annotation_table.md)
  : Example annotation table
- [`k562_count_table`](https://emanuelsoda.github.io/ScreenR/reference/k562_count_table.md)
  : K562 shRNA essentiality screen: count table
- [`k562_annotation_table`](https://emanuelsoda.github.io/ScreenR/reference/k562_annotation_table.md)
  : K562 shRNA essentiality screen: annotation table
- [`k562_reference_genes`](https://emanuelsoda.github.io/ScreenR/reference/k562_reference_genes.md)
  : K562 shRNA essentiality screen: reference essential and
  non-essential genes
- [`hap1_count_table`](https://emanuelsoda.github.io/ScreenR/reference/hap1_count_table.md)
  : HAP1 CRISPR-Cas9 knockout screen: count table
- [`hap1_annotation_table`](https://emanuelsoda.github.io/ScreenR/reference/hap1_annotation_table.md)
  : HAP1 CRISPR-Cas9 knockout screen: annotation table

## Package

- [`ScreenR-package`](https://emanuelsoda.github.io/ScreenR/reference/ScreenR-package.md)
  [`ScreenR`](https://emanuelsoda.github.io/ScreenR/reference/ScreenR-package.md)
  : ScreenR: analysis of pooled shRNA screens

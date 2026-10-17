# Create a ScreenR object

Creates the ScreenR object, the starting point of every analysis, from a
count table and an annotation table. The object stores the counts, the
annotation, the groups of the samples and, later, the normalized counts
and the data table.

## Usage

``` r
create_screenr_object(
  table = NULL,
  annotation = NULL,
  groups = NULL,
  replicates = c(""),
  barcode_column = "Barcode",
  gene_column = "Gene",
  sample_table = NULL,
  sample_columns = c(Sample = "Sample", Day = "Day", Treatment = "Treatment"),
  duplicate_barcodes = c("error", "sum", "first"),
  duplicate_annotation = c("error", "first")
)
```

## Arguments

- table:

  The count table obtained from the read alignment, with one row for
  each barcode and one column for each sample, plus the barcode column.

- annotation:

  The annotation table, with one row for each barcode and at least the
  barcode column and the gene column (the gene targeted by the shRNA).

- groups:

  A factor with the experimental group of each sample (for example
  "Control" and "Treated"), with one value for each sample (column of
  the count table other than the barcode column). It is used by the
  edgeR/limma based methods, see
  [`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md).

- replicates:

  An optional vector with the replicate labels. When it has one value
  for each sample, it is stored in the Replicate column of the colData.
  Default c("").

- barcode_column:

  The name of the column containing the barcode identifiers, in both the
  count and the annotation table. Default "Barcode".

- gene_column:

  The name of the column of the annotation table containing the gene
  names. Default "Gene".

- sample_table:

  An optional data frame with one row for each sample and columns giving
  the sample name, the Day (time point) and the Treatment of the sample.
  When given, these values are used instead of the ones derived from the
  sample names.

- sample_columns:

  A named character vector giving the names of the columns of
  sample_table that contain the sample names, the Day and the Treatment.
  Default c(Sample = "Sample", Day = "Day", Treatment = "Treatment").

- duplicate_barcodes:

  What to do when a barcode appears more than once in the count table:
  "error" (default), "sum" (add up the counts of the copies) or "first"
  (keep the first copy).

- duplicate_annotation:

  What to do when a barcode is annotated more than once with different
  values: "error" (default) or "first" (keep the first row).

## Value

A ScreenR object (class
[`screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/screenr_object-class.md)),
not normalized: run
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
before the analyses that need normalized counts.

## Details

The tables can use any column names: the names of the barcode and gene
columns are given with barcode_column and gene_column, and the object
renames them internally to Barcode and Gene (the original names are
kept, see
[`get_column_names`](https://emanuelsoda.github.io/ScreenR/reference/get_column_names.md)).
When a column is not found, the error suggests the most similar column
name.

The input is checked when the object is created:

- the barcode identifiers are converted to character (numbers to their
  decimal form: 10208 and "10208" are the same barcode) and become the
  row names of the counts; they never become a count column. Missing or
  empty identifiers are an error;

- sample columns stored as text but containing only numbers are
  converted to numbers;

- other non-numeric columns of the count table (for example a gene
  column) are removed with a message, since they are not samples;

- missing, infinite or negative counts are an error;

- duplicated barcodes are an error, unless duplicate_barcodes says how
  to combine them;

- barcodes annotated more than once with different values are an error,
  unless duplicate_annotation = "first" (rows that are exact copies are
  merged silently);

- groups must have one value for each sample;

- barcodes missing from the annotation (for example the unmapped reads
  "\*") give a warning.

The object stores the raw counts as `assay(object, "counts")`, the
annotation of the barcodes in its rowData and the sample information in
its colData (see
[`screenr_object-class`](https://emanuelsoda.github.io/ScreenR/reference/screenr_object-class.md)).
Annotation rows of barcodes that are not in the count table are kept
apart and returned by
[`get_annotation_table`](https://emanuelsoda.github.io/ScreenR/reference/get_annotation_table.md).

The Day (time point) and the Treatment of each sample are used by
several functions. By default they are taken from the sample names,
which must have the form Day_Treatment_Replicate (for example
Time3_TRT_A). When the sample names have another form, give them
explicitly with sample_table, and check the result with
[`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md).

## See also

[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md),
[`compute_data_table`](https://emanuelsoda.github.io/ScreenR/reference/compute_data_table.md),
[`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md),
[`get_count_table`](https://emanuelsoda.github.io/ScreenR/reference/get_count_table.md)
and the other accessors.

## Examples

``` r
count_table <-
    data.frame(
        Barcode = c("Code_1", "Code_2", "Code_3", "Code_4"),
        Time3_CTRL_rep1 = c(3520, 3020, 1507, 1400),
        Time3_CTRL_rep2 = c(3500, 3000, 1457, 1490),
        Time3_TRT_rep1 = c(1200, 1100, 1300, 1350),
        Time3_TRT_rep2 = c(1250, 1000, 1400, 1375)
    )
annotation_table <-
    data.frame(
        Gene = c("Gene_1", "Gene_1", "Gene_2", "Gene_2"),
        Barcode = c("Code_1", "Code_2", "Code_3", "Code_4")
    )

groups <- factor(c("Control", "Control", "Treated", "Treated"))
obj <- create_screenr_object(
    table = count_table,
    annotation = annotation_table,
    groups = groups, replicates = c("")
)
obj
#> class: screenr_object 
#> dim: 4 4 
#> metadata(1): screenr_layout
#> assays(1): counts
#> rownames(4): Code_1 Code_2 Code_3 Code_4
#> rowData names(2): Barcode Gene
#> colnames(4): Time3_CTRL_rep1 Time3_CTRL_rep2 Time3_TRT_rep1
#>   Time3_TRT_rep2
#> colData names(4): Sample Day Treatment Group
#> normalization: none (run normalize_data())

# Tables with other column names and sample names without the
# Day_Treatment_Replicate form
counts <- count_table
colnames(counts) <- c("shRNA", "c1", "c2", "t1", "t2")
annotation <- data.frame(
    symbol = annotation_table$Gene,
    shRNA = annotation_table$Barcode
)
samples <- data.frame(
    id = c("c1", "c2", "t1", "t2"),
    time = "Day3",
    condition = c("CTRL", "CTRL", "TRT", "TRT")
)
obj <- create_screenr_object(
    table = counts, annotation = annotation, groups = groups,
    barcode_column = "shRNA", gene_column = "symbol",
    sample_table = samples,
    sample_columns = c(Sample = "id", Day = "time",
        Treatment = "condition")
)
get_sample_table(obj)
#> # A tibble: 4 × 3
#>   Sample Day   Treatment
#>   <chr>  <chr> <chr>    
#> 1 c1     Day3  CTRL     
#> 2 c2     Day3  CTRL     
#> 3 t1     Day3  TRT      
#> 4 t2     Day3  TRT      
get_column_names(obj)
#>     Barcode        Gene      Sample         Day   Treatment 
#>     "shRNA"    "symbol"        "id"      "time" "condition" 
```

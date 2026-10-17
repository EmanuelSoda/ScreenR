# The ScreenR object class

The screenr_object class is the main object of the package: it stores
the data of a screen and is passed to the functions of the analysis.
Create it with
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

It extends
[`SummarizedExperiment`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html),
which is the single source of truth of the object:

- `assay(object, "counts")`: the raw counts, one row for each barcode
  and one column for each sample. They are never modified by the
  analysis. The row names are the barcode identifiers (as character) and
  the column names the samples.

- `rowData(object)`: the annotation of each barcode (Gene, ...), aligned
  with the rows of the counts. Its Barcode column is kept equal to the
  row names, for compatibility.

- `colData(object)`: the Sample, Day and Treatment of each sample (see
  [`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md)),
  its Group, its Replicate (when replicate labels were given for each
  sample) and, after
  [`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md),
  its effective library size (EffectiveLibSize).

- `metadata(object)$normalization`: the method of the normalization and
  its provenance (see
  [`get_normalization`](https://emanuelsoda.github.io/ScreenR/reference/get_normalization.md)).

The normalized counts, the data table and the edgeR objects are not
stored: they are computed when needed from the raw counts, the
annotation, the sample information and the effective library sizes, so
they always agree with each other and with the counts (see
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)).

## Value

`screenr_object()` (the class generator) returns a new object of class
`screenr_object`; use
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)
to build one from your data.

## Details

**Changing the object.** The ScreenR setters
([`set_count_table`](https://emanuelsoda.github.io/ScreenR/reference/set_count_table.md),
[`set_annotation_table`](https://emanuelsoda.github.io/ScreenR/reference/set_annotation_table.md),
[`set_groups`](https://emanuelsoda.github.io/ScreenR/reference/set_groups.md),
...) and the replacement functions of SummarizedExperiment keep the
object consistent:

- replacing the counts (`set_count_table`,
  `assay(object, "counts") <- `, `assays(object) <- `) with different
  values removes the normalization, which must be computed again with
  [`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md);

- replacing the annotation (`set_annotation_table`,
  `rowData(object) <- `) changes the gene of each barcode used by all
  the following analyses;

- `rowData<-`, `colData<-` (and `$<-`) and `dimnames<-` check the
  object, and renaming the barcodes updates the Barcode column of the
  rowData;

- subsetting and reordering (`object[i, j]`) keep the effective library
  sizes of the retained samples: the normalization is not computed again
  on the retained barcodes. Call
  [`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
  after filtering to normalize on the retained barcodes.

Modifying the slots directly (`object@assays <- ...`) bypasses these
safeguards and is not supported. Combining objects with `cbind` or
`rbind` is only meaningful between objects normalized in the same way;
the result is checked by `validObject()`.

## Slots

- `annotation_table`:

  The rows of the annotation table given by the user whose barcodes are
  not in the count table (barcodes of the library that were not
  observed). The annotation of the observed barcodes is in the rowData.
  [`get_annotation_table`](https://emanuelsoda.github.io/ScreenR/reference/get_annotation_table.md)
  returns both.

- `replicates`:

  The replicate labels, when they were not given for each sample
  (sample-specific labels are in the Replicate column of the colData).

- `data_table`:

  Not used any more (the data table is computed when needed, see
  [`get_data_table`](https://emanuelsoda.github.io/ScreenR/reference/get_data_table.md));
  kept, empty, so that saved objects can be read.

- `column_names`:

  The names that the columns had in the tables given by the user (see
  [`get_column_names`](https://emanuelsoda.github.io/ScreenR/reference/get_column_names.md)).

## See also

[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md),
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md),
[`get_normalization`](https://emanuelsoda.github.io/ScreenR/reference/get_normalization.md),
[`update_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/update_screenr_object.md)

## Examples

``` r
data("count_table", package = "ScreenR")
data("annotation_table", package = "ScreenR")

groups <- factor(c(
    "T1/T2", "T1/T2", "Treated", "Treated", "Treated",
    "Control", "Control", "Control", "Treated", "Treated",
    "Treated", "Control", "Control", "Control"
))

# The unmapped reads ("*") appear several times: remove them
counts <- count_table[count_table$Barcode != "*", ]
obj <- create_screenr_object(
    table = counts,
    annotation = annotation_table,
    groups = groups
)
#> The Day and the Treatment of each sample are taken from the sample names (Day_Treatment_Replicate). For 2 sample(s) (Time_1, Time_2) they cannot be separated and are both set to the first part of the name. If this is not intended, give them with the sample_table argument. Check them with get_sample_table().
obj
#> class: screenr_object 
#> dim: 5320 14 
#> metadata(1): screenr_layout
#> assays(1): counts
#> rownames(5320): 10208 10502 ... 13876 13995
#> rowData names(5): Barcode Gene Gene_ID Sequence Library
#> colnames(14): Time_1 Time_2 ... Time_4_rep2 Time_4_rep3
#> colData names(4): Sample Day Treatment Group
#> normalization: none (run normalize_data())
SummarizedExperiment::assay(obj, "counts")[1:3, 1:3]
#>       Time_1 Time_2 Time_3_TRT_rep1
#> 10208   3520   4283            2493
#> 10502    280    339             140
#> 10796   1507   1533            1120
SummarizedExperiment::rowData(obj)
#> DataFrame with 5320 rows and 5 columns
#>           Barcode        Gene   Gene_ID  Sequence   Library
#>       <character> <character> <logical> <logical> <logical>
#> 10208       10208    Gene_380        NA        NA        NA
#> 10502       10502    Gene_380        NA        NA        NA
#> 10796       10796    Gene_380        NA        NA        NA
#> 11089       11089    Gene_380        NA        NA        NA
#> 11382       11382    Gene_380        NA        NA        NA
#> ...           ...         ...       ...       ...       ...
#> 13520       13520    Gene_534        NA        NA        NA
#> 13638       13638    Gene_534        NA        NA        NA
#> 13757       13757    Gene_534        NA        NA        NA
#> 13876       13876    Gene_534        NA        NA        NA
#> 13995       13995    Gene_534        NA        NA        NA

# Normalized values are derived from the counts and the normalization
obj <- normalize_data(obj, method = "tmm")
get_normalization(obj)$effective_library_size
#>          Time_1          Time_2 Time_3_TRT_rep1 Time_3_TRT_rep2 Time_3_TRT_rep3 
#>        10853404         9827013         6840595         9229604         9737088 
#>     Time_3_rep1     Time_3_rep2     Time_3_rep3 Time_4_TRT_rep1 Time_4_TRT_rep2 
#>         9647737         9179789         9641856         8799318         9717098 
#> Time_4_TRT_rep3     Time_4_rep1     Time_4_rep2     Time_4_rep3 
#>        12044656        10302865         9128760         9182589 
```

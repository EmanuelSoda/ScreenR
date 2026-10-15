# The ScreenR object class

The screenr_object class is the main object of the package: it stores
the data of a screen and is passed to the functions of the analysis.
Create it with
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

It extends
[`SummarizedExperiment`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html),
so it can be used with all the Bioconductor tools that work with that
class:

- `assay(object, "counts")`: the raw counts, one row for each barcode
  and one column for each sample;

- `assay(object, "normalized")`: the normalized counts, after
  [`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md);

- `rowData(object)`: the barcode identifier (Barcode) and its annotation
  (Gene, ...);

- `colData(object)`: the Sample, Day and Treatment of each sample (see
  [`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md))
  and its Group.

The ScreenR accessors
([`get_count_table`](https://emanuelsoda.github.io/ScreenR/reference/get_count_table.md),
[`set_count_table`](https://emanuelsoda.github.io/ScreenR/reference/set_count_table.md),
...) work as in the previous versions of the package. Objects created
with versions older than 1.15.8 are converted automatically (see
[`update_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/update_screenr_object.md)).

## Value

`screenr_object()` (the class generator) returns a new object of class
`screenr_object`; use
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)
to build one from your data.

## Slots

- `annotation_table`:

  The annotation table as given by the user (it can contain barcodes
  that are not in the count table).

- `replicates`:

  The replicate labels.

- `data_table`:

  The data table, a tidy (long) version of the normalized counts (see
  [`compute_data_table`](https://emanuelsoda.github.io/ScreenR/reference/compute_data_table.md)).

- `column_names`:

  The names that the columns had in the tables given by the user (see
  [`get_column_names`](https://emanuelsoda.github.io/ScreenR/reference/get_column_names.md)).

## See also

[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md),
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

obj <- create_screenr_object(
    table = count_table,
    annotation = annotation_table,
    groups = groups,
    replicates = c("")
)
#> Warning: The count table has duplicated barcodes (*).
#> Warning: 1 barcode(s) of the count table are not in the annotation table (*). Remove them (for example the unmapped reads "*") before the edgeR based analyses.
#> The Day and the Treatment of each sample are taken from the sample names (Day_Treatment_Replicate). For 2 sample(s) (Time_1, Time_2) they cannot be separated and are both set to the first part of the name. If this is not intended, give them with the sample_table argument. Check them with get_sample_table().
obj
#> class: screenr_object 
#> dim: 5323 14 
#> metadata(0):
#> assays(1): counts
#> rownames(5323): 10208 10502 ... 13995 *.2
#> rowData names(5): Barcode Gene Gene_ID Sequence Library
#> colnames(14): Time_1 Time_2 ... Time_4_rep2 Time_4_rep3
#> colData names(4): Sample Day Treatment Group
#> data table: 0 rows, 0 columns
```

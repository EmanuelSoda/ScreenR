# Update a ScreenR object

Converts a ScreenR object created with an older version of ScreenR to
the current layout. All the ScreenR functions do this automatically
(with a message), so it is only needed to save the converted object, or
to use it with other Bioconductor tools.

## Usage

``` r
update_screenr_object(object, verbose = TRUE)
```

## Arguments

- object:

  A ScreenR object.

- verbose:

  Whether to give a message when the object is converted. Default TRUE.

## Value

A ScreenR object of the current layout.

## Details

Two kinds of older objects are converted:

- objects created before version 1.15.8, which were not
  SummarizedExperiment objects;

- objects created by versions 1.15.8 to 1.15.15, which stored the
  normalized counts as an assay and the data table as a slot.

The counts, the annotation, the groups and the sample information are
kept. The normalized counts and the data table of older objects are
**not** kept: those versions did not record how the counts were
normalized, and the edgeR based methods ignored the normalization, so it
cannot be recovered reliably. Run
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
on the converted object (with the method used originally, "cpm" by
default). Objects of the current layout are returned unchanged.

## See also

[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md),
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
object <- update_screenr_object(object)
object
#> class: screenr_object 
#> dim: 5320 14 
#> metadata(2): screenr_layout normalization
#> assays(1): counts
#> rownames(5320): 10208 10502 ... 13876 13995
#> rowData names(5): Barcode Gene Gene_ID Sequence Library
#> colnames(14): T1 T2 ... Time4_B Time4_c
#> colData names(5): Sample Day Treatment Group EffectiveLibSize
#> normalization: cpm (effective library sizes in colData(object)$EffectiveLibSize)
```

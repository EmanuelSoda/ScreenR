# Create an edgeR object

Converts a ScreenR object into an edgeR
[`DGEList`](https://rdrr.io/pkg/edgeR/man/DGEList.html), for example to
use the object with edgeR or limma functions directly. All the
edgeR/limma based functions of ScreenR
([`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
[`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
[`compute_barcode_statistics`](https://emanuelsoda.github.io/ScreenR/reference/compute_barcode_statistics.md),
[`find_time_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md),
[`filter_by_variance`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_variance.md)
and
[`plot_barcode_hit`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md))
use it, so they all model the counts with the same normalization.

## Usage

``` r
create_edger_obj(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md),
  normalized with
  [`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md).

## Value

A [`DGEList`](https://rdrr.io/pkg/edgeR/man/DGEList.html) with the raw
counts, the effective library sizes, the groups of the samples and the
annotation of the barcodes (genes).

## Details

The DGEList contains the raw counts, as required by the edgeR negative
binomial model (never normalized counts), and the normalization of the
object set by
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md):
the library sizes (lib.size) are the effective library sizes of the
samples and the normalization factors (norm.factors) are 1. The offsets
of the models, log(lib.size \* norm.factors), are therefore the
logarithms of the effective library sizes, the same that give the
normalized counts of the plots and of the data table (raw count /
effective library size \* 1e6). For the "tmm" method the effective
library size already includes the TMM factor, so it is not applied a
second time; do not call
[`normLibSizes`](https://rdrr.io/pkg/edgeR/man/calcNormFactors.html) (or
calcNormFactors) on the result, which would change the normalization.

The object must be normalized, and every barcode must be annotated with
a gene. The genes table of the DGEList is the rowData of the object.

## See also

[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md),
[`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
dge <- create_edger_obj(object)
dge$samples
#>               group lib.size norm.factors
#> T1            T1/T2 10671757            1
#> T2            T1/T2  9799724            1
#> Time3_TRT_A Treated  6824877            1
#> Time3_TRT_B Treated  9107354            1
#> Time3_TRT_C Treated  9717868            1
#> Time3_A     Control  9485195            1
#> Time3_B     Control  9016812            1
#> Time3_C     Control  9568624            1
#> Time4_TRT_A Treated  8933915            1
#> Time4_TRT_B Treated  9883209            1
#> Time4_TRT_C Treated 12325124            1
#> Time4_A     Control 10351136            1
#> Time4_B     Control  9180192            1
#> Time4_c     Control  9315365            1
```

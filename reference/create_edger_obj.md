# Create an edgeR object

Converts a ScreenR object into an edgeR
[`DGEList`](https://rdrr.io/pkg/edgeR/man/DGEList.html), for example to
use the object with edgeR or limma functions directly. The edgeR/limma
based functions of ScreenR (such as
[`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)
and
[`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md))
call it internally. The DGEList is built from the raw counts (the count
table), as required by the edgeR negative binomial model, and the
annotation is matched to the counts by barcode; all the barcodes of the
count table must be in the annotation table.

## Usage

``` r
create_edger_obj(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

A [`DGEList`](https://rdrr.io/pkg/edgeR/man/DGEList.html) with the raw
counts, the groups of the samples and the annotation of the barcodes
(genes).

## See also

[`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
create_edger_obj(object)
#> An object of class "DGEList"
#> $counts
#>         T1   T2 Time3_TRT_A Time3_TRT_B Time3_TRT_C Time3_A Time3_B Time3_C
#> 10208 3520 4283        2493        3998        3018    3393    3829    3474
#> 10502  280  339         140         542         162     468     157     162
#> 10796 1507 1533        1120        1125        1286    1136    1095    1214
#> 11089 3797 4151        2296        3857        3645    3677    3124    3458
#> 11382  717  644         564         336         256     554     590     735
#>       Time4_TRT_A Time4_TRT_B Time4_TRT_C Time4_A Time4_B Time4_c
#> 10208        2890        3776        5630    3500    3288    3640
#> 10502         290         152         418     654     270     189
#> 10796         127         395         625     452     378     842
#> 11089        2321        3221        3461    3726    3104    2561
#> 11382         319         106         401     127     403     666
#> 5315 more rows ...
#> 
#> $samples
#>               group lib.size norm.factors
#> T1            T1/T2 10671757            1
#> T2            T1/T2  9799724            1
#> Time3_TRT_A Treated  6824877            1
#> Time3_TRT_B Treated  9107354            1
#> Time3_TRT_C Treated  9717868            1
#> 9 more rows ...
#> 
#> $genes
#>           Gene Barcode Gene_ID Sequence Library
#> 10208 Gene_380   10208      NA       NA      NA
#> 10502 Gene_380   10502      NA       NA      NA
#> 10796 Gene_380   10796      NA       NA      NA
#> 11089 Gene_380   11089      NA       NA      NA
#> 11382 Gene_380   11382      NA       NA      NA
#> 5315 more rows ...
#> 
```

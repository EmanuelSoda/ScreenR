# Count the barcodes of each gene

Counts the barcodes (shRNAs or sgRNAs) of each gene in the annotation of
a ScreenR object. Use it to choose `number_barcode` (in
[`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
[`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
[`find_time_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md)
and
[`plot_barcode_hit`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_hit.md))
and `min_barcodes` (in
[`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md)):
shRNA libraries often have 5 to 25 barcodes per gene, CRISPR libraries 3
to 6 sgRNAs per gene (4 in Brunello and TKOv3).

## Usage

``` r
barcodes_per_gene(screenR_Object)
```

## Arguments

- screenR_Object:

  A ScreenR object created with
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

A tibble with the columns Gene and NBarcodes, ordered by gene.

## See also

[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
per_gene <- barcodes_per_gene(object)
head(per_gene)
#> # A tibble: 6 × 2
#>   Gene     NBarcodes
#>   <chr>        <int>
#> 1 Gene_1          10
#> 2 Gene_10         10
#> 3 Gene_100        10
#> 4 Gene_101        10
#> 5 Gene_102        10
#> 6 Gene_103        10
table(per_gene$NBarcodes)
#> 
#>   7   8   9  10  12  32 
#>   1   1  41 488   2   1 
```

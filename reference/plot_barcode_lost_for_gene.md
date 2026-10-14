# Plot number of barcode lost for gene

This function plots the number of barcodes lost in each sample for each
gene. Usually in a genetic screening each gene is is associated with
multiple shRNAs and so barcodes. For this reason a reasonable number of
barcodes associated with the gene has to be retrieved in order to have a
robust result. Visualizing the number of genes that have lost lot's of
barcode is a Quality Check procedure in order to be aware of the number
of barcode for the hit identified.

## Usage

``` r
plot_barcode_lost_for_gene(screenR_Object, facet = TRUE, samples)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

- facet:

  A boolean to use the facet.

- samples:

  A vector of samples that as to be visualize

## Value

Return the plot displaying the number of barcode lost for each gene in
each sample.

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

plot_barcode_lost_for_gene(object,
    samples = c("Time3_A", "Time3_B")
)

plot_barcode_lost_for_gene(object,
    samples = c("Time3_A", "Time3_B"),
    facet = FALSE
)
```

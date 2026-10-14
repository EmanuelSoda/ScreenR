# Find Camera Hit

This function implements the method by proposed by Wu and Smyth (2012).
The original [`camera`](https://rdrr.io/pkg/limma/man/camera.html)
method is a gene set test, here is applied in the contest of a genetic
screening and so it erforms a competitive barcode set test. The paper
can be found here
[CAMERA](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3458527/)

## Usage

``` r
find_camera_hit(
  screenR_Object,
  matrix_model,
  contrast,
  number_barcode = 3,
  thresh = 1e-04,
  lfc = 1,
  direction = "Down"
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

- matrix_model:

  The matrix that will be used to perform the linear model analysis
  created using
  [`model.matrix`](https://rdrr.io/r/stats/model.matrix.html)

- contrast:

  A vector or a single value indicating the index or the name of the
  column the model_matrix with which perform the analysis

- number_barcode:

  Number of barcode that as to be differentially expressed (DE)in order
  to consider the gene associated DE. Example a gene is associated with
  10 shRNA we consider a gene DE if it has at least number_barcode = 5
  shRNA DE.

- thresh:

  The threshold for the False Discovery Rate (FDR) that has to be used
  to select the statistically significant hits.

- lfc:

  The Log2FC threshold.

- direction:

  String containing the direction of the variation, "Down" for the down
  regulation "Up" for the up regulation.

## Value

The data frame containing the hit found using the camera method

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

matrix <- model.matrix(~ slot(object, "groups"))
colnames(matrix) <- c("Control", "T1/T2", "Treated")

result <- find_camera_hit(
    screenR_Object = object,
    matrix_model = matrix, contrast = "Treated"
)
#> Warning: 3rows with all zero counts
#> Warning: Zero sample variances detected, have been offset away from zero
head(result)
#> # A tibble: 6 × 5
#>   Gene     NGenes Direction    PValue    FDR
#>   <chr>     <dbl> <fct>         <dbl>  <dbl>
#> 1 Gene_173     10 Down      0.0000311 0.0166
#> 2 Gene_15      10 Down      0.00389   0.413 
#> 3 Gene_352     10 Down      0.00709   0.495 
#> 4 Gene_121     10 Down      0.0115    0.540 
#> 5 Gene_377     10 Down      0.0123    0.540 
#> 6 Gene_422     10 Down      0.0131    0.540 
```

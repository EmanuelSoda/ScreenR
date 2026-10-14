# Find Roast Hit

Find the hit using the roast method. Roast is a competitive gene set
test which uses rotation instead of permutation. Here is applied in a
contest of a genetic screening so it perform a barcode competitive test
testing for barcode which are differentially expressed within a gene.
More information can be found in
[Roast](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2922896/)

## Usage

``` r
find_roast_hit(
  screenR_Object,
  matrix_model,
  contrast,
  nrot = 9999,
  number_barcode = 3,
  direction = "Down",
  p_val = 0.05
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

- matrix_model:

  The matrix that will be used to perform the linear model analysis.
  Created using
  [`model.matrix`](https://rdrr.io/r/stats/model.matrix.html)

- contrast:

  A vector or a single value indicating the index or the name of the
  column the model_matrix to which perform the analysis

- nrot:

  Number of rotation to perform the test. Higher number of rotation
  leads to more statistically significant result.

- number_barcode:

  Number of barcode that as to be differentially expressed (DE)in order
  to consider the gene associated DE. Example a gene is associated with
  10 shRNA we consider a gene DE if it has at least number_barcode = 5
  shRNA DE.

- direction:

  Direction of variation

- p_val:

  The value that as to be used as p-value cut off

## Value

The hits found by ROAST method

## Examples

``` r
set.seed(42)
object <- get0("object", envir = asNamespace("ScreenR"))
matrix_model <- model.matrix(~ slot(object, "groups"))
colnames(matrix_model) <- c("Control", "T1_T2", "Treated")

result <- find_roast_hit(object,
    matrix_model = matrix_model,
    contrast = "Treated", nrot = 100
)
#> Warning: 3rows with all zero counts
head(result)
#> # A tibble: 6 × 9
#>   Gene     NGenes PropDown PropUp Direction  PValue   FDR PValue.Mixed FDR.Mixed
#>   <chr>     <int>    <dbl>  <dbl> <fct>       <dbl> <dbl>        <dbl>     <dbl>
#> 1 Gene_173     10      0.7    0   Down      0.00990 0.264      0.00990     0.610
#> 2 Gene_15      10      0.4    0   Down      0.00990 0.264      0.00990     0.610
#> 3 Gene_121     10      0.4    0   Down      0.00990 0.264      0.00990     0.610
#> 4 Gene_293     10      0.4    0.1 Down      0.00990 0.264      0.0198      0.610
#> 5 Gene_394     10      0.3    0   Down      0.00990 0.264      0.366       0.848
#> 6 Gene_446     10      0.2    0   Down      0.00990 0.264      0.347       0.848
```

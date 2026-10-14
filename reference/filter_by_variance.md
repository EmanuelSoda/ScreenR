# Filter using the variance filter

This function is used to improve the quality of the hits. It compute the
variance among the hits and filter the one with a value greater than the
threshold set

## Usage

``` r
filter_by_variance(
  screenR_Object,
  genes,
  matrix_model,
  variance = 0.5,
  contrast
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

- genes:

  The genes for which the variance as to be computed. Those genes are
  the result of the three statistical methods selection

- matrix_model:

  a matrix created using
  [`model.matrix`](https://rdrr.io/r/stats/model.matrix.html)

- variance:

  The maximum value of variance accepted

- contrast:

  The variable to use as X for the linear model for the Treatment

## Value

A data frame with the variance for the treatment and the control for
each gene

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
matrix_model <- model.matrix(~ slot(object, "groups"))
colnames(matrix_model) <- c("Control", "T1_T2", "Treated")
contrast <- limma::makeContrasts(Treated - Control, levels = matrix_model)

data <- filter_by_variance(
    screenR_Object = object, genes = c("Gene_42"),
    matrix_model = matrix_model, contrast = contrast
)
head(data)
#> # A tibble: 6 × 10
#>   Barcode Gene    Sample      Frequency Sequence Library Gene_ID Day   Treatment
#>   <fct>   <chr>   <fct>           <dbl> <lgl>    <lgl>   <lgl>   <chr> <chr>    
#> 1 12937   Gene_42 T1               471. NA       NA      NA      T1    T1       
#> 2 12937   Gene_42 T2               406. NA       NA      NA      T2    T2       
#> 3 12937   Gene_42 Time3_TRT_A      365. NA       NA      NA      Time3 TRT      
#> 4 12937   Gene_42 Time3_TRT_B      448. NA       NA      NA      Time3 TRT      
#> 5 12937   Gene_42 Time3_TRT_C      394. NA       NA      NA      Time3 TRT      
#> 6 12937   Gene_42 Time3_A          459. NA       NA      NA      Time3 Time3    
#> # ℹ 1 more variable: Variance <dbl>
```

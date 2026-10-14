# Filter using the slope filter

This function is used to improve the quality of the hits found. It
computes a regression line in the different samples ad uses the slope of
this line to see the trend

## Usage

``` r
filter_by_slope(
  screenR_Object,
  genes,
  group_var_treatment,
  group_var_control,
  slope_control,
  slope_treatment
)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

- genes:

  The genes for which the slope as to be computed. Those genes are the
  result of the three statistical methods selection

- group_var_treatment:

  The variable to use as independent variable (x) for the linear model
  of the treatment

- group_var_control:

  The variable to use as independent variable (x) for the linear model
  of the the control

- slope_control:

  A value used as threshold for the control slope

- slope_treatment:

  A value used as threshold for the treatment slope

## Value

A data frame with the slope for the treatment and the control for each
gene

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

filter_by_slope(
    screenR_Object = object, genes = c("Gene_1", "Gene_2"),
    group_var_treatment = c("T1", "T2", "TRT"),
    group_var_control = c("T1", "T2", "Time3", "Time4"),
    slope_control = 0.5, slope_treatment = 1
)
#> [1]        NA  2.163455 -3.866243
#> # A tibble: 1 × 3
#>   Gene   slope_control slope_treatment
#>   <chr>          <dbl>           <dbl>
#> 1 Gene_2         -3.87           -4.07
```

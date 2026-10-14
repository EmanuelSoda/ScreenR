# Get ScreenR count table

Get function for the count table of the ScreenR object

## Usage

``` r
get_count_table(object)

# S4 method for class 'screenr_object'
get_count_table(object)
```

## Arguments

- object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Value

The count table of the ScreenR object

## Slots

- `count_table`:

  It is used to store the count table to perform the analysis

- `annotation_table`:

  It is used to store the annotation of the shRNA

- `groups`:

  It is used to store the vector of treated and untreated

- `replicates`:

  It is used to store information about the replicates

- `normalized_count_table`:

  It is used to store a normalized version of the count table

- `data_table`:

  It is used to store a tidy format of the count table

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
count_table <- get_count_table(object)
#> ScreenR count table containing:
#>  5320 rows
#>  15 columns
head(count_table)
#> # A tibble: 6 × 15
#>   Barcode    T1    T2 Time3_TRT_A Time3_TRT_B Time3_TRT_C Time3_A Time3_B
#>   <fct>   <int> <int>       <int>       <int>       <int>   <int>   <int>
#> 1 10208    3520  4283        2493        3998        3018    3393    3829
#> 2 10502     280   339         140         542         162     468     157
#> 3 10796    1507  1533        1120        1125        1286    1136    1095
#> 4 11089    3797  4151        2296        3857        3645    3677    3124
#> 5 11382     717   644         564         336         256     554     590
#> 6 11675    7330  7593        4927        6211        6504    7781    7183
#> # ℹ 7 more variables: Time3_C <int>, Time4_TRT_A <int>, Time4_TRT_B <int>,
#> #   Time4_TRT_C <int>, Time4_A <int>, Time4_B <int>, Time4_c <int>
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
```

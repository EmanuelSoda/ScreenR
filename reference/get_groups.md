# Get the groups

Returns the experimental group of each sample of a ScreenR object, as
given to
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).
The groups define the columns of the design matrix
([`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md)).

## Usage

``` r
get_groups(object)

# S4 method for class 'screenr_object'
get_groups(object)
```

## Arguments

- object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

A factor with one value for each sample.

## See also

[`set_groups`](https://emanuelsoda.github.io/ScreenR/reference/set_groups.md),
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
groups <- get_groups(object)
groups
#>  [1] T1/T2   T1/T2   Treated Treated Treated Control Control Control Treated
#> [10] Treated Treated Control Control Control
#> Levels: Control T1/T2 Treated
```

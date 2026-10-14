# Get ScreenR groups

Get function for the groups of the ScreenR object

## Usage

``` r
get_groups(object)

# S4 method for class 'screenr_object'
get_groups(object)
```

## Arguments

- object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Value

The groups of the ScreenR object

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
groups <- get_groups(object)
groups
#>  [1] T1/T2   T1/T2   Treated Treated Treated Control Control Control Treated
#> [10] Treated Treated Control Control Control
#> Levels: Control T1/T2 Treated
```

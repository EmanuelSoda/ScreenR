# Get ScreenR replicates

Get function for the replicates of the ScreenR object

## Usage

``` r
get_replicates(object)

# S4 method for class 'screenr_object'
get_replicates(object)
```

## Arguments

- object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Value

The replicates of the ScreenR object

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
replicates <- get_replicates(object)
replicates
#> [1] ""
```

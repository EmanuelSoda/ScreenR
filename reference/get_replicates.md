# Get the replicates

Returns the replicate labels stored in a ScreenR object.

## Usage

``` r
get_replicates(object)

# S4 method for class 'screenr_object'
get_replicates(object)
```

## Arguments

- object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

The vector of replicate labels.

## See also

[`set_replicates`](https://emanuelsoda.github.io/ScreenR/reference/set_replicates.md),
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
replicates <- get_replicates(object)
replicates
#> [1] ""
```

# Multidimensional Scaling Plot

Plot samples on a two-dimensional scatterplot so that distances on the
plot approximate the typical log2 fold changes between the samples.

## Usage

``` r
plot_mds(
  screenR_Object,
  groups = NULL,
  alpha = 0.8,
  size = 2.5,
  color = "black"
)
```

## Arguments

- screenR_Object:

  The Object of the package
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

- groups:

  The vector that has to be used to fill the plot if NULL the function
  will use the default groups slot in the object passed as input.

- alpha:

  The opacity of the labels. Possible value are in a range from 0 to 1.

- size:

  The dimension of the labels. The default value is 2.5

- color:

  The color of the labels. The default value is black

## Value

The MDS Plot

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

plot_mds(object)
```

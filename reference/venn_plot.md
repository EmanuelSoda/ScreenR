# Draw a Venn Diagram

Internal function that draws a Venn diagram of one to three sets of
genes with ggplot2.

## Usage

``` r
venn_plot(
  sets,
  show_elements,
  show_percentage,
  max_elements,
  alpha,
  stroke_size,
  set_name_size,
  text_color,
  text_size,
  color
)
```

## Arguments

- sets:

  A named list of one to three gene sets.

- show_elements, show_percentage, max_elements, alpha, stroke_size,
  set_name_size, text_color, text_size, color:

  See
  [`plot_common_hit`](https://emanuelsoda.github.io/ScreenR/reference/plot_common_hit.md).

## Value

A ggplot object.

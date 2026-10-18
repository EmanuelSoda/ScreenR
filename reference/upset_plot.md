# Draw an UpSet Plot

Internal function that draws an UpSet plot of several sets of genes with
ggplot2 and patchwork.

## Usage

``` r
upset_plot(sets, n_intersections, text_size, color, title)
```

## Arguments

- sets:

  A named list of gene sets.

- n_intersections:

  The largest number of combinations shown.

- text_size:

  The size of the counts.

- color:

  The color of the bars and dots.

- title:

  The title of the plot.

## Value

A patchwork object.

# Color Scale of a Plot

Internal function that returns the color scale of a plot colored by a
sample variable: the explicit palette when given, otherwise the palette
of the object for the variable (when it gives a color to every value
shown), otherwise NULL (the default colors of ggplot2).

## Usage

``` r
palette_scale(object, variable, values, palette = NULL, aesthetic = "fill")
```

## Arguments

- object:

  A ScreenR object, or NULL.

- variable:

  The sample variable that colors the plot.

- values:

  The values shown in the plot.

- palette:

  An explicit palette (vector of colors), or NULL.

- aesthetic:

  "fill" or "colour".

## Value

A ggplot2 scale, or NULL.

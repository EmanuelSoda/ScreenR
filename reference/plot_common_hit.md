# Plot the hits in common between methods

Draws a Venn diagram of the hits of the three methods (Z-score, CAMERA
and ROAST), to see how many hits they have in common. It is a wrapper
around
[`ggvenn`](https://yanlinlin82.github.io/ggvenn/reference/ggvenn.html).

## Usage

``` r
plot_common_hit(
  hit_zscore,
  hit_camera,
  hit_roast,
  alpha = 0.5,
  stroke_size = 0.5,
  set_name_size = 4,
  text_color = "black",
  text_size = 4,
  show_percentage = TRUE,
  title = "",
  color = c("#1B9E77", "#D95F02", "#7570B3"),
  show_elements = TRUE
)
```

## Arguments

- hit_zscore:

  The hits of the Z-score method, a table with a Gene column (for
  example from
  [`find_gene_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_gene_zscore_hit.md)).

- hit_camera:

  The hits of
  [`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
  a table with a Gene column.

- hit_roast:

  The hits of
  [`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md),
  a table with a Gene column.

- alpha:

  The opacity of the circles, between 0 and 1. Default 0.5.

- stroke_size:

  The width of the circle borders.

- set_name_size:

  The text size of the set names.

- text_color:

  The text color of the contents of the intersections.

- text_size:

  The text size of the contents of the intersections.

- show_percentage:

  Whether to show the percentage of each set (used when show_elements is
  FALSE).

- title:

  The title displayed above the plot.

- color:

  The three colors used to fill the circles.

- show_elements:

  Whether to show the gene names instead of the counts (default TRUE).

## Value

A ggplot object.

## See also

[`find_common_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_common_hit.md),
[`summarise_hits`](https://emanuelsoda.github.io/ScreenR/reference/summarise_hits.md)

## Examples

``` r
hit_zscore <- data.frame(Gene = c("A", "B", "C", "D", "E"))
hit_camera <- data.frame(Gene = c("A", "B", "C", "F", "H", "G"))
hit_roast <- data.frame(Gene = c("A", "L", "N"))
plot_common_hit(hit_zscore, hit_camera, hit_roast)


# Number of genes instead of their names
plot_common_hit(hit_zscore, hit_camera, hit_roast, show_elements = FALSE)
```

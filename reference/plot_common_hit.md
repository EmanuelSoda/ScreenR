# Plot common hit

This method plot the hits in common among the three methods is a wrapper
for the
[`ggvenn`](https://yanlinlin82.github.io/ggvenn/reference/ggvenn.html)
function.

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

  The list of hits of the
  [`find_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md)

- hit_camera:

  The list of hits of the
  [`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md)

- hit_roast:

  The list of hits of the
  [`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md)

- alpha:

  A value for the opacity of the plot. Allowed values are in the range 0
  to 1

- stroke_size:

  Stroke size for drawing circles

- set_name_size:

  Text size for set names

- text_color:

  Text color for intersect contents

- text_size:

  Text size for intersect contents

- show_percentage:

  Show percentage for each set

- title:

  The title to display above the plot

- color:

  The three vector color for the venn

- show_elements:

  Show set elements instead of count/percentage.

## Value

A vector containing the common hit

## Examples

``` r
hit_zscore <- data.frame(Gene = c("A", "B", "C", "D", "E"))
hit_camera <- data.frame(Gene = c("A", "B", "C", "F", "H", "G"))
hit_roast <- data.frame(Gene = c("A", "L", "N"))
plot_common_hit(hit_zscore, hit_camera, hit_roast)

```

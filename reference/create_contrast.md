# Create a contrast

Creates the contrast that compares the treatment group(s) with the
control group(s) of a design matrix created with
[`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md).
When more than one group is given on one side, their average is used.

## Usage

``` r
create_contrast(design, treatment, control)
```

## Arguments

- design:

  A design matrix created with
  [`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md).

- treatment:

  The name(s) of the treatment group(s), as in the groups of the ScreenR
  object or as in the column names of the design matrix.

- control:

  The name(s) of the control group(s), as in the groups of the ScreenR
  object or as in the column names of the design matrix.

## Value

A named numeric vector with one value for each column of the design
matrix: 1 / (number of treatment groups) for the treatment groups, -1 /
(number of control groups) for the control groups and 0 for the others.

## See also

[`create_design_matrix`](https://emanuelsoda.github.io/ScreenR/reference/create_design_matrix.md),
[`find_camera_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_camera_hit.md),
[`find_roast_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_roast_hit.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
design <- create_design_matrix(object)
create_contrast(design, treatment = "Treated", control = "Control")
#> Control   T1.T2 Treated 
#>      -1       0       1 
```

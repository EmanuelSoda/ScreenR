# Analyse a screen in the browser with the ScreenR app

Opens an interactive application, built with Shiny, that runs the main
steps of a ScreenR analysis without writing code: loading the counts (an
example screen, a count table with its annotation, or a MAGeCK count
file), describing the samples, quality control, finding the hits with
several methods and checking each hit at the level of its barcodes.
Tables and plots can be downloaded, together with an R script that
repeats the analysis.

The app needs the packages shiny, bslib and DT (install them with
`BiocManager::install(c("shiny", "bslib", "DT"))`).

## Usage

``` r
run_screenr_app(object = NULL, ...)

screenr_app(object = NULL)
```

## Arguments

- object:

  An optional ScreenR object to open in the app, for example one created
  with
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).
  If `NULL` the app starts from the data page.

- ...:

  Further arguments passed to
  [`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html), such as
  `launch.browser` or `port`.

## Value

`run_screenr_app()` runs the app and returns when it is closed.
`screenr_app()` returns the app (a `shiny.appobj`), which can be run
with [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) or
deployed to a Shiny server.

## Examples

``` r
if (interactive()) {
    run_screenr_app()

    # Open an object that is already in the session
    data(count_table)
    data(annotation_table)
    object <- create_screenr_object(
        table = count_table, annotation = annotation_table,
        groups = factor(c(
            "T1/T2", "T1/T2", "Time3_TRT", "Time3_TRT", "Time3_TRT",
            "Time3_CTRL", "Time3_CTRL", "Time3_CTRL", "Time4_TRT",
            "Time4_TRT", "Time4_TRT", "Time4_CTRL", "Time4_CTRL",
            "Time4_CTRL"
        ))
    )
    run_screenr_app(object)
}
```

#' @title Analyse a screen in the browser with the ScreenR app
#' @description Opens an interactive application, built with Shiny, that
#'   runs the main steps of a ScreenR analysis without writing code: loading
#'   the counts (an example screen, a count table with its annotation, or a
#'   MAGeCK count file), describing the samples, quality control, finding the
#'   hits with several methods and checking each hit at the level of its
#'   barcodes. Tables and plots can be downloaded, together with an R script
#'   that repeats the analysis.
#'
#'   The app needs the packages shiny, bslib and DT (install them with
#'   \code{BiocManager::install(c("shiny", "bslib", "DT"))}).
#' @param object An optional ScreenR object to open in the app, for example
#'   one created with \code{\link{create_screenr_object}}. If \code{NULL} the
#'   app starts from the data page.
#' @param ... Further arguments passed to \code{\link[shiny]{runApp}}, such
#'   as \code{launch.browser} or \code{port}.
#' @return \code{run_screenr_app()} runs the app and returns when it is
#'   closed. \code{screenr_app()} returns the app (a \code{shiny.appobj}),
#'   which can be run with \code{shiny::runApp()} or deployed to a Shiny
#'   server.
#' @export
#' @examples
#' if (interactive()) {
#'     run_screenr_app()
#'
#'     # Open an object that is already in the session
#'     data(count_table)
#'     data(annotation_table)
#'     object <- create_screenr_object(
#'         table = count_table, annotation = annotation_table,
#'         groups = factor(c(
#'             "T1/T2", "T1/T2", "Time3_TRT", "Time3_TRT", "Time3_TRT",
#'             "Time3_CTRL", "Time3_CTRL", "Time3_CTRL", "Time4_TRT",
#'             "Time4_TRT", "Time4_TRT", "Time4_CTRL", "Time4_CTRL",
#'             "Time4_CTRL"
#'         ))
#'     )
#'     run_screenr_app(object)
#' }
run_screenr_app <- function(object = NULL, ...) {
    shiny::runApp(screenr_app(object), ...)
}

#' @rdname run_screenr_app
#' @export
screenr_app <- function(object = NULL) {
    missing <- c("shiny", "bslib", "DT")[!vapply(c("shiny", "bslib", "DT"),
        requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing) > 0) {
        stop("The ScreenR app needs the package(s) ",
            paste(missing, collapse = ", "), ". Install them with ",
            "BiocManager::install(c(\"",
            paste(missing, collapse = "\", \""), "\"))",
            call. = FALSE)
    }
    if (!is.null(object) && !methods::is(object, "screenr_object")) {
        stop("object must be a ScreenR object", call. = FALSE)
    }
    shiny::addResourcePath("screenr-app",
        system.file("app", "www", package = "ScreenR"))
    shiny::shinyApp(ui = app_ui(), server = app_server(object))
}

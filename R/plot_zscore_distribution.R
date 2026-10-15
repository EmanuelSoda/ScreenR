#' @title Plot the distribution of the barcode log2 fold changes
#' @description Draws the density of the log2 fold changes of the barcodes
#'     (treated versus control), with one curve for each time point. It is
#'     useful to compare the strength of the effect of the treatment at the
#'     different time points.
#' @param time_point_measure A list with the tables of
#'     \code{\link{compute_metrics}} for the different time points (or a
#'     single table). Each table contains, for each barcode, the columns
#'     Log2FC and Day.
#' @param alpha The opacity of the densities, between 0 and 1. Default 1.
#' @importFrom rlang .data
#' @return A ggplot object.
#' @seealso \code{\link{compute_metrics}}, \code{\link{plot_barcode_trend}}
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' table1 <- compute_metrics(object,
#'     control = "Time3", treatment = "TRT",
#'     day = "Time3"
#' )
#'
#' table2 <- compute_metrics(object,
#'     control = "Time4", treatment = "TRT",
#'     day = "Time4"
#' )
#'
#' plot_zscore_distribution(list(table1, table2), alpha = 0.5)
plot_zscore_distribution <- function(time_point_measure, alpha = 1) {
  dplyr::bind_rows(time_point_measure) %>%
    ggplot(aes(x = .data$Log2FC, fill = .data$Day)) +
    geom_density(alpha = alpha)
}

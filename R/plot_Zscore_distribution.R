#' @title Plot distribution Z-score
#' @description This function plots the Log2FC Z-score distribution of the
#'              treated vs control in the different time points.
#'
#' @param time_point_measure A list containing the table for each time
#'                           point
#' @param alpha A value for the opacity of the plot.
#'              Allowed values are in the range 0 to 1
#' @importFrom rlang .data
#' @return return the density plot of the distribution of the Z-score
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' table1 <- compute_metrics(object,
#'     control = "TRT", treatment = "Time3",
#'     day = "Time3"
#' )
#'
#' table2 <- compute_metrics(object,
#'     control = "TRT", treatment = "Time4",
#'     day = "Time4"
#' )
#'
#' plot_zscore_distribution(list(table1, table2), alpha = 0.5)
#'
plot_zscore_distribution <- function(time_point_measure, alpha = 1) {
    dplyr::bind_rows(time_point_measure) %>%
        ggplot(aes(x = .data$Log2FC, fill = .data$Day)) +
        geom_density(alpha = alpha)
}

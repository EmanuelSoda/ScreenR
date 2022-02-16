#' @title Plot distribution Z-score
#' @description This function plot the distribution of the Z-score of the
#'              Log2FC of the treated vs control in the different timepoint
#'
#' @param time_point_measure A list containing the table for each time
#'                           point
#' @param alpha the alpha value for the plot
#' @importFrom rlang .data
#' @return return the density plot of the distribution of the Z-score
#' @export
#' @examples
#' obj <- get0('obj', envir = asNamespace('ScreenR'))
#' table1 <- compute_metrics(obj, control = 'Met', treatment = 'DMSO',
#'                          day = 'Day3')
#' table2 <- compute_metrics(obj, control = 'Met', treatment = 'DMSO',
#'                          day = 'Day6')
#'
#'plot_Zscore_distribution(list(table1, table2), alpha = 0.5)


plot_Zscore_distribution <- function(time_point_measure, alpha = 1) {
    dplyr::bind_rows(time_point_measure) %>%
        ggplot(aes(x = .data$Log2FC, fill = .data$Day)) +
        geom_density(alpha = alpha)
}

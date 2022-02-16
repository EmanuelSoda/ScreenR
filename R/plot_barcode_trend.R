#' @title Plot the trend over time of the Barcode
#' @description Plot the log2FC over time of the barcode in the different
#' time point
#' @importFrom rlang .data
#' @param list_data_measure A list containing the measure table of the
#'                          different time point
#' @param n_col The number of col to use in the facette wrap
#' @param size_line the dimension of the line
#' @param genes vector of genes name
#' @param color vector of colors
#' @return The the plot of the
#' @export
#'

plot_barcode_trend <- function(list_data_measure, genes,
    n_col = 2, size_line = 1, color = NULL) {

    data <- bind_rows(list_data_measure) %>%
        filter(.data$Gene %in% genes) %>%
        mutate(Day = factor(x = .data$Day, levels = unique(.data$Day)))
    if (is.null(color)) {
        gg_l <- map(.x = split(data, f = as.character(data$Gene)),
            .f = function(x) {
                ggplot(x, aes(x = .data$Day, y = .data$Log2FC,
                  group = .data$Barcode, col = .data$Barcode)) +
                  geom_line(size = size_line) + geom_point() +
                  theme_light() + facet_wrap(facets = "Gene",
                  scales = "free")
            })
    } else {
        gg_l <- map(.x = split(data, f = as.character(data$Gene)),
            .f = function(x) {
                ggplot(x, aes(x = .data$Day, y = .data$Log2FC,
                  group = .data$Barcode, col = .data$Barcode)) +
                  geom_line(size = size_line) + geom_point() +
                  scale_color_manual(values = color) +
                  facet_wrap(facets = "Gene", scales = "free")
            })
    }


    patchwork::wrap_plots(gg_l, ncol = n_col)

}

#' @title Plot the trend over time of the Barcodes
#' @description Plot the log2FC over time of the barcodes in the different
#'              time point
#' @importFrom rlang .data
#' @param list_data_measure A list containing the measure table of the
#'                          different time point
#' @param n_col The number of column to use in the facet wrap
#' @param size_line Rhe dimension of the line
#' @param genes Vector of genes name
<<<<<<< HEAD
#' @param color A color palette for the plot
=======
#' @param color Vector of colors
>>>>>>> develop_2
#' @return The rend plot for the genes in input
#' @export
#'

plot_barcode_trend <- function(list_data_measure, genes,
    n_col = 2, size_line = 1, color = NULL) {

    data <- bind_rows(list_data_measure) %>%
        filter(.data$Gene %in% genes) %>%
        mutate(Day = factor(x = .data$Day, levels = unique(.data$Day)))
    if (is.null(color)) {
        gg_l <- map(.x = split(data, f = as.character(data$Gene)),
            .f = ~ggplot(.x, aes(x = .data$Day, y = .data$Log2FC,
                    group = .data$Barcode, col = .data$Barcode)) +
                    geom_line(size = size_line) + geom_point() +
                    theme_light() + facet_wrap(facets = "Gene",
                    scales = "free")
            )
    } else {
        gg_l <- map(.x = split(data, f = as.character(data$Gene)),
            .f = ~ggplot(.x, aes(x = .data$Day, y = .data$Log2FC,
                    group = .data$Barcode, col = .data$Barcode)) +
                    geom_line(size = size_line) + geom_point() +
                    scale_color_manual(values = color) +
                    facet_wrap(facets = "Gene", scales = "free")
            )
    }


    patchwork::wrap_plots(gg_l, ncol = n_col)

}

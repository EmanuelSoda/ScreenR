#' @title Plot the trend over time of the Barcodes
#' @description Plot the log2FC over time of the barcodes in the different
#'              time point
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows
#' @importFrom patchwork wrap_plots
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom purrr map reduce
#' @importFrom ggplot2 geom_line geom_point facet_wrap scale_color_manual
#' @param list_data_measure A list containing the measure table of the
#'                          different time point
#' @param n_col The number of column to use in the facet wrap
#' @param size_line Rhe dimension of the line
#' @param genes Vector of genes name
#' @param color Vector of colors
#' @return The rend plot for the genes in input
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' metrics <- dplyr::bind_rows(
#'     compute_metrics(object,
#'         control = "TRT", treatment = "Time3",
#'         day = "Time3"
#'     ),
#'     compute_metrics(object,
#'         control = "TRT", treatment = "Time4",
#'         day = "Time4"
#'     )
#' )
#' # Multiple Genes
#' plot_barcode_trend(metrics,
#'     genes = c("Gene_1", "Gene_50"),
#'     n_col = 2
#' )
#' # Single Gene
#' plot_barcode_trend(metrics, genes = "Gene_300")
plot_barcode_trend <- function(list_data_measure, genes,
    n_col = 1, size_line = 1, color = NULL) {
    data <- dplyr::bind_rows(list_data_measure) %>%
        filter(.data$Gene %in% genes) %>%
        mutate(Day = factor(x = .data$Day, levels = unique(.data$Day)))
    if (is.null(color)) {
        gg_l <- purrr::map(
            .x = split(data, f = as.character(data$Gene)),
            .f = ~ ggplot(.x, aes(
                x = .data$Day, y = .data$Log2FC,
                group = .data$Barcode, col = .data$Barcode
            )) +
                geom_line(size = size_line) +
                geom_point() +
                facet_wrap(
                    facets = "Gene",
                    scales = "free"
                )
        )
    } else {
        gg_l <- map(
            .x = split(data, f = as.character(data$Gene)),
            .f = ~ ggplot(.x, aes(
                x = .data$Day, y = .data$Log2FC,
                group = .data$Barcode, col = .data$Barcode
            )) +
                geom_line(size = size_line) +
                geom_point() +
                scale_color_manual(values = color) +
                facet_wrap(facets = "Gene", scales = "free")
        )
    }
    if (n_col > 1) {
        gg_l <- patchwork::wrap_plots(gg_l, ncol = n_col)
    }

    return(gg_l)
}

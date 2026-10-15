#' @title Plot the trend of the barcodes of genes over time
#' @description Plots the log2 fold change of each barcode of the given genes
#'     at the different time points, one line for each barcode. It is useful
#'     to check the efficacy of each shRNA: good shRNAs of a hit should have a
#'     consistent trend over time.
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows
#' @importFrom patchwork wrap_plots
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom purrr map reduce
#' @importFrom ggplot2 geom_line geom_point facet_wrap scale_color_manual
#' @param list_data_measure The tables of \code{\link{compute_metrics}} for the
#'     different time points, as a list or already combined with
#'     \code{\link[dplyr]{bind_rows}}. The time points are shown in the order
#'     in which they appear.
#' @param n_col The number of columns used to arrange the plots of the genes
#'     (default 1). When larger than 1 the plots are combined with
#'     \code{\link[patchwork]{wrap_plots}}.
#' @param size_line The width of the lines. Default 1.
#' @param genes The names of the genes to plot.
#' @param color An optional vector of colors, one for each barcode.
#' @return With n_col = 1, a list with one ggplot object for each gene; with
#'     n_col > 1, a single patchwork object combining them.
#' @seealso \code{\link{compute_metrics}}, \code{\link{plot_barcode_hit}},
#'     \code{\link{find_discordant_barcodes}}
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' metrics <- dplyr::bind_rows(
#'     compute_metrics(object,
#'         control = "Time3", treatment = "TRT",
#'         day = "Time3"
#'     ),
#'     compute_metrics(object,
#'         control = "Time4", treatment = "TRT",
#'         day = "Time4"
#'     )
#' )
#' # Multiple genes
#' plot_barcode_trend(metrics,
#'     genes = c("Gene_1", "Gene_50"),
#'     n_col = 2
#' )
#' # Single gene
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
                facet_wrap(facets = "Gene", scales = "free")
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

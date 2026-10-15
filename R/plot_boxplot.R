#' @title Plot the normalized counts of genes in each sample
#' @description Draws, for the given genes, a box plot (or violin plot) of the
#'     normalized counts of their barcodes in each sample, taken from the data
#'     table. It shows the overall trend of a gene and whether it is depleted
#'     or enriched.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param group_var The values of the Treatment column of the data table (see
#'     \code{\link{get_sample_table}}) of the samples to plot.
#' @param fill_var The column of the data table used to fill the boxes, for
#'     example "Sample" (default), "Day" or "Treatment".
#' @param alpha Not used.
#' @param genes The names of the genes to plot.
#' @param nrow The number of rows of panels, when several genes are plotted.
#' @param ncol The number of columns of panels, when several genes are
#'     plotted.
#' @param type The type of plot: "boxplot" (default) or "violinplot".
#' @param scales The scales of the panels: "free" (default), "fixed",
#'     "free_x" or "free_y".
#' @concept  plot
#' @importFrom dplyr sym
#' @importFrom ggplot2 geom_violin
#' @return A ggplot object.
#' @seealso \code{\link{plot_trend}}, \code{\link{compute_data_table}}
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' plot_boxplot(object,
#'     genes = c("Gene_34"),
#'     group_var = c("T1", "T2", "TRT"), nrow = 1, ncol = 2,
#'     fill_var = "Day", type = "violinplot"
#' )
plot_boxplot <- function(screenR_Object, genes, group_var, alpha = 0.5,
    nrow = 1, ncol = 1, fill_var = "Sample",
    type = "boxplot", scales = "free") {
    data <- screenR_Object@data_table

    # Select only the hit gene
    data <- dplyr::filter(data, .data$Gene %in% genes)

    # Select only the sample of interest
    data <- dplyr::filter(data, .data$Treatment %in% group_var)

    # Order the fill legend as the samples, not alphabetically (T0, T3, T18
    # and not T0, T18, T3)
    if (is.character(data[[fill_var]])) {
        data[[fill_var]] <- factor(data[[fill_var]],
            levels = unique(data[[fill_var]][order(data$Sample)]))
    }

    plot <- ggplot2::ggplot(data, aes(.data$Sample, .data$Frequency,
        fill = !!sym(fill_var)
    ))
    if (type == "boxplot") {
        plot <- plot + geom_boxplot()
    } else if (type == "violinplot") {
        plot <- plot + geom_violin()
    } else {
        stop("The only available type of plot are boxplot and violinplot")
    }

    if (length(genes) > 1) {
        plot <- plot + facet_wrap("Gene",
            nrow = nrow,
            ncol = ncol, scales = scales
        )
    }

    return(plot)
}

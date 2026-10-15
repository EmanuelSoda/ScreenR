#' @title Plot the trend of genes over the samples
#' @description Plots, for the given genes, the mean normalized count of their
#'     barcodes in each sample (taken from the data table), with a regression
#'     line. It shows whether a hit decreases (or increases) over time in the
#'     selected samples.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param group_var The values of the Treatment column of the data table (see
#'     \code{\link{get_sample_table}}) of the samples to plot, for example the
#'     early time points and the treated samples.
#' @param alpha The opacity of the confidence band, between 0 and 1. Default
#'     0.5.
#' @param se Whether to draw the confidence band of the regression line.
#'     Default FALSE.
#' @param point_size The size of the points. Default 1.
#' @param line_size The width of the regression line. Default 1.
#' @param nrow The number of rows of panels, when several genes are plotted.
#' @param ncol The number of columns of panels, when several genes are
#'     plotted.
#' @param genes The names of the genes to plot.
#' @param scales The scales of the panels: "free" (default), "fixed",
#'     "free_x" or "free_y".
#' @return A ggplot object.
#' @seealso \code{\link{compute_slope}}, \code{\link{filter_by_slope}},
#'     \code{\link{plot_boxplot}}
#' @importFrom ggplot2 geom_smooth
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' plot_trend(object, genes = "Gene_42", group_var = c("T1", "T2", "TRT"))
#'
#' plot_trend(object,
#'     genes = c("Gene_42", "Gene_100"),
#'     group_var = c("T1", "T2", "TRT"),
#'     nrow = 2
#' )
plot_trend <- function(screenR_Object, genes, group_var,
    alpha = 0.5, se = FALSE, point_size = 1, line_size = 1,
    nrow = 1, ncol = 1, scales = "free") {
    data <- compute_trend(screenR_Object, genes, group_var)

    plot <- ggplot2::ggplot(data, aes(.data$Sample, .data$Frequency)) +
        ggplot2::geom_point(size = point_size) +
        ggplot2::geom_smooth(aes(group = .data$Gene),
            method = "lm", formula = y ~ x, alpha = alpha,
            se = se, size = line_size
        )

    if (length(genes) > 1) {
        plot <- plot +
            ggplot2::facet_wrap("Gene",
                nrow = nrow, ncol = ncol,scales = scales)
    }

    return(plot)
}

#' @title Compute the trend of genes
#' @description Internal function that computes, for each gene and sample, the
#'     mean normalized count of the barcodes of the gene.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param genes The names of the genes.
#' @param group_var The values of the Treatment column of the samples to use.
#' @return A tibble with the columns Sample, Gene and Frequency.
#' @keywords internal
compute_trend <- function(screenR_Object, genes, group_var) {
    data <- screenR_Object@data_table

    # Select only the hit gene
    data <- dplyr::filter(data, .data$Gene %in% genes)

    # Select only the sample of interest
    data <- dplyr::filter(data, .data$Treatment %in% group_var)

    data <- dplyr::group_by(data, .data$Sample, .data$Gene)

    # Consider only the gene (which are the mean of the different shRNAs)
    data <- dplyr::summarise(data,
        Gene = unique(.data$Gene),
        Sample = unique(.data$Sample),
        Frequency = mean(.data$Frequency),
        .groups = "drop"
    )

    return(data)
}

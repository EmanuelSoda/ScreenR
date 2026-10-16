#' @title Plot the trend of genes over time
#' @description Plots, for the given genes, the mean normalized count of their
#'     barcodes in each sample (taken from the data table) against the time
#'     point of the sample, with a regression line. It shows whether a hit
#'     decreases (or increases) over time in the selected samples. The slope
#'     of the line is the one computed by \code{\link{compute_slope}} and
#'     used by \code{\link{filter_by_slope}}.
#' @details Replicates of the same time point are drawn at the same time. By
#'     default the time points are numbered 1, 2, ... in the order of the
#'     samples (equally spaced); give time to place them at their real times.
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
#'     plotted. By default (NULL) ggplot2 chooses nrow and ncol.
#' @param genes The names of the genes to plot.
#' @param scales The scales of the panels: "free" (default), "fixed",
#'     "free_x" or "free_y".
#' @param time_column The column of the data table with the time point of
#'     each sample: "Day" (default) or, for example, "Treatment" when the
#'     time points are described there.
#' @param time An optional named numeric vector with the time of each time
#'     point (the names are the values of time_column), for example
#'     c(T0 = 0, T3 = 3, T18 = 18). By default the time points are equally
#'     spaced, in the order of the samples.
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
#'
#' # With the real times of the time points
#' plot_trend(object,
#'     genes = "Gene_42", group_var = c("T1", "T2", "TRT"),
#'     time = c(T1 = 0, T2 = 2, Time3 = 7, Time4 = 14)
#' )
plot_trend <- function(screenR_Object, genes, group_var,
    alpha = 0.5, se = FALSE, point_size = 1, line_size = 1,
    nrow = NULL, ncol = NULL, scales = "free", time_column = "Day",
    time = NULL) {
    data <- compute_trend(screenR_Object, genes, group_var, time_column, time)

    plot <- ggplot2::ggplot(data, aes(.data$Time, .data$Frequency)) +
        ggplot2::geom_point(size = point_size) +
        ggplot2::geom_smooth(aes(group = .data$Gene),
            method = "lm", formula = y ~ x, alpha = alpha,
            se = se, linewidth = line_size
        ) +
        ggplot2::labs(y = "Mean normalized counts")

    if (is.null(time)) {
        # Equally spaced time points: label them with their names
        labels <- attr(data$Time, "labels")
        plot <- plot + ggplot2::scale_x_continuous(
            name = time_column, breaks = seq_along(labels), labels = labels)
    } else {
        plot <- plot + ggplot2::scale_x_continuous(name = "Time",
            breaks = sort(unique(data$Time)))
    }

    if (length(genes) > 1) {
        plot <- plot +
            ggplot2::facet_wrap("Gene",
                nrow = nrow, ncol = ncol,scales = scales)
    }

    return(plot)
}

#' @title Compute the trend of genes
#' @description Internal function that computes, for each gene and sample, the
#'     mean normalized count of the barcodes of the gene, and the time of the
#'     sample (see \code{\link{encode_time}}).
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param genes The names of the genes.
#' @param group_var The values of the Treatment column of the samples to use.
#' @param time_column The column of the data table with the time point of
#'     each sample.
#' @param time NULL or a named numeric vector with the time of each time
#'     point.
#' @return A tibble with the columns Sample, Gene, Frequency and Time.
#' @keywords internal
compute_trend <- function(screenR_Object, genes, group_var,
    time_column = "Day", time = NULL) {
    data <- screenR_Object@data_table

    # Select only the hit gene
    data <- dplyr::filter(data, .data$Gene %in% genes)

    # Select only the sample of interest
    data <- dplyr::filter(data, .data$Treatment %in% group_var)
    data$Time <- encode_time(data, time_column, time)
    labels <- attr(data$Time, "labels")

    # Consider only the gene (which are the mean of the different shRNAs)
    data <- dplyr::summarise(
        dplyr::group_by(data, .data$Sample, .data$Gene),
        Time = unique(.data$Time),
        Frequency = mean(.data$Frequency),
        .groups = "drop"
    )
    attr(data$Time, "labels") <- labels

    return(data)
}

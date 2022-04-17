#' @title Plot the trend Hit Gene
#' @description This function plot the trend of a gene resulted as hit
#' @importFrom rlang .data
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @param group_var The variable that as to be used to filter the data, for
#'                  example the different treatment
#' @param alpha A value for the opacity of the plot.
#'              Allowed values are in the range 0 to 1
#' @param se A boolean to indicate where or not to plot the standard error
#' @param point_size The dimension of each dot
#' @param line_size The dimension of the line
#' @param nrow The number of rows in case multiple genes are plotted
#' @param ncol The number of columns in case multiple genes are plotted
#' @param genes The vector of genes to use
#' @return The the plot of the
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
plot_trend <- function(screenR_Object, genes, group_var,
    alpha = 0.5, se = FALSE, point_size = 1, line_size = 1,
    nrow = 1, ncol = 1) {
    data <- screenR_Object@data_table

    # Select only the hit gene
    data <- dplyr::filter(data, .data$Gene %in% genes)

    # Select only the sample of interest
    data <- dplyr::filter(data, .data$Treatment %in%
        group_var)

    data <- dplyr::group_by(data, .data$Sample, .data$Gene)

    # Consider only the gene (which are the mean of the different shRNAs)
    data <- dplyr::summarise(data,
        Gene = unique(.data$Gene),
        Sample = unique(.data$Sample), Frequency = mean(.data$Frequency),
        .groups = "drop"
    )

    plot <- ggplot2::ggplot(data, aes(
        .data$Sample,
        .data$Frequency
    )) +
        ggplot2::geom_point(size = point_size) +
        ggplot2::geom_smooth(aes(group = .data$Gene),
            method = "lm", formula = y ~ x, alpha = alpha,
            se = se, size = line_size
        )

    if (length(genes) > 1) {
        plot <- plot + ggplot2::facet_wrap("Gene",
            nrow = nrow,
            ncol = ncol
        )
    }

    return(plot)
}

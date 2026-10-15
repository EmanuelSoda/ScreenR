#' @title Plot the distribution of the mapped reads
#' @description Draws a box plot or a density plot of the number of reads of
#'     the barcodes in each sample. It is a quality control plot: samples of
#'     good quality have roughly the same distribution.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param palette An optional vector of colors, one for each sample. By default
#'     the ggplot2 colors are used.
#' @param alpha The opacity of the boxes or densities, between 0 and 1.
#'     Default 1.
#' @param type The type of plot: "boxplot" (default) or "density".
#' @importFrom ggplot2 geom_boxplot geom_density
#' @concept plot
#' @return A ggplot object.
#' @seealso \code{\link{count_mapped_reads}}, \code{\link{plot_mapped_reads}}
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' # Box plot
#' plot_mapped_reads_distribution(object)
#'
#' # Density
#' plot_mapped_reads_distribution(object, type = "density")
#'
#' # Density with transparency
#' plot_mapped_reads_distribution(object, type = "density", alpha = 0.2)
plot_mapped_reads_distribution <- function(screenR_Object,
    palette = NULL, alpha = 1, type = "boxplot") {
    table <- ScreenR::count_mapped_reads(screenR_Object)

    if (type == "boxplot") {
        plot <- ggplot(data = table, aes(
            x = .data$Sample,
            y = .data$Mapped, fill = .data$Sample
        )) +
            ggplot2::geom_boxplot(alpha = alpha)
    } else if (type == "density") {
        plot <- ggplot(data = table, aes(
            x = .data$Mapped,
            fill = .data$Sample
        )) +
            ggplot2::geom_density(alpha = alpha)
    }

    if (!is.null(palette)) {
        plot <- plot + scale_fill_manual(values = palette)
    }
    return(plot)
}

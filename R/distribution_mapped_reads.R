#' @title Plot the distribution of the mapped reads
#' @description Draws a box plot or a density plot of the number of reads of
#'     the barcodes in each sample. It is a quality control plot: samples of
#'     good quality have roughly the same distribution.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param palette An optional vector of colors for the samples (named by
#'     sample, or one for each sample in order), which replaces the palette
#'     of the object. By default the Sample palette of the object is used
#'     when it is set (see \code{\link{get_palettes}}), otherwise the
#'     default colors of ggplot2.
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
    object <- update_screenr_object(screenR_Object)
    table <- ScreenR::count_mapped_reads(object)

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

    if (type == "boxplot") {
        plot <- plot + ggplot2::labs(y = "Reads per barcode")
    } else if (type == "density") {
        plot <- plot + ggplot2::labs(x = "Reads per barcode", y = "Density")
    }
    plot <- plot + palette_scale(object, "Sample", table$Sample, palette)
    return(plot)
}

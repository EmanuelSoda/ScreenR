#' @title Plot the distribution of the mapped reads
#' @description This function creates a boxplot to show the distribution of
#'              the mapped reads in different samples.
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}.
#'
#' @param palette The color vector for the plot.
#' @param alpha A value for the opacity of the plot.
#'              Allowed values are in the range 0 to 1
#' @param type The type of plot. The default is "boxplot" the other option is
#'             "density."
#' @importFrom ggplot2 geom_boxplot geom_density
#' @concept plot
#' @return Return a tibble containing the number of mapped read for sample
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' # Boxplot
#' distribution_mapped_reads(object)
#'
#' # Density
#' distribution_mapped_reads(object, type = "density")
#'
#  # Density changing also alpha
#' distribution_mapped_reads(object, type = "density", alpha = 0.2)
#'
distribution_mapped_reads <- function(screenR_Object,
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

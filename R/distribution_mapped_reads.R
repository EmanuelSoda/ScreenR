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
#' @concept plot
#'
#' @return Return a tibble containing the number of mapped read for sample
#' @export
#'
#' @examples
#' obj <- get0('obj', envir = asNamespace('ScreenR'))
#'
#' distribution_mapped_reads(obj)
#'
#' distribution_mapped_reads(obj, type = 'density')
#'
#' distribution_mapped_reads(obj, type = 'density', alpha = 0.5)

distribution_mapped_reads <- function(screenR_Object,
    palette = NULL, alpha = 1, type = "boxplot") {
    table <- count_mapped_reads(screenR_Object)

    if (type == "boxplot")
        plot <- ggplot(data = table, aes(x = .data$Sample,
            y = .data$Mapped, fill = .data$Sample)) +
            geom_boxplot(alpha = alpha) else if (type == "density") {
        plot <- ggplot(data = table, aes(x = .data$Mapped,
            fill = .data$Sample)) + geom_density(alpha = alpha)
    }

    if (!is.null(palette)) {
        plot <- plot + scale_fill_manual(values = palette)
    }
    return(plot)
}

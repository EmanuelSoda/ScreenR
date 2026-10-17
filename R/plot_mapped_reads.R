#' @title Plot the number of mapped reads of each sample
#' @description Draws a bar plot of the total number of reads mapped to the
#'     barcodes in each sample, computed with \code{\link{mapped_reads}}.
#'     Samples with far fewer reads than the others may have a lower quality.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param palette An optional vector of colors for the samples (named by
#'     sample, or one for each sample in order), which replaces the palette
#'     of the object. By default the Sample palette of the object is used
#'     when it is set (see \code{\link{get_palettes}}), otherwise the
#'     default colors of ggplot2.
#' @param alpha The opacity of the bars, between 0 and 1. Default 1.
#' @param legende_position The position of the legend: "none" (default),
#'     "top", "bottom", "right" or "left".
#' @importFrom magrittr %>%
#' @importFrom  ggplot2 geom_bar
#' @importFrom rlang .data
#' @return A ggplot object.
#' @seealso \code{\link{mapped_reads}},
#'     \code{\link{plot_mapped_reads_distribution}},
#'     \code{\link{plot_barcode_lost}}
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' plot_mapped_reads(object)
plot_mapped_reads <- function(screenR_Object, palette = NULL, alpha = 1,
    legende_position = "none") {
    object <- update_screenr_object(screenR_Object)
    table <- ScreenR::mapped_reads(object)
    plot <- ggplot(table, aes(
        x = .data$Sample, y = .data$Mapped,
        fill = .data$Sample
    )) +
        geom_bar(alpha = alpha, stat = "identity")
    plot <- plot + palette_scale(object, "Sample", table$Sample, palette)
    plot <- plot + ggplot2::labs(y = "Mapped reads")
    plot <- plot + theme(legend.position = legende_position)
    return(plot)
}

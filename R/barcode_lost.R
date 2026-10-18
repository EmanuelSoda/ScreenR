#' @title Compute the number of barcodes lost in each sample
#' @description Counts, for each sample, the barcodes lost during the screen
#'     or the sequencing, that is the barcodes with zero mapped reads. Many
#'     lost barcodes may indicate a sample of low quality or a bottleneck in
#'     the screen. Use \code{\link{plot_barcode_lost}} to plot the result.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @importFrom  dplyr group_by
#' @importFrom  dplyr filter
#' @importFrom  dplyr summarise
#' @importFrom  magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr vars
#' @importFrom tidyr drop_na
#' @concept compute
#' @return A tibble with the columns Sample and LostBarcode (the number of
#'     barcodes with zero reads). Samples without lost barcodes are not
#'     reported.
#' @seealso \code{\link{plot_barcode_lost}},
#'     \code{\link{plot_barcode_lost_for_gene}},
#'     \code{\link{count_mapped_reads}}
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' head(barcode_lost(object))
#'
#' @export
barcode_lost <- function(screenR_Object) {
    table <- ScreenR::count_mapped_reads(screenR_Object)
    table <- table %>%
        dplyr::group_by(.data$Sample) %>%
        dplyr::filter(.data$Mapped == 0) %>%
        dplyr::summarise(LostBarcode = n())

    return(table)
}

#' @title Plot the number of barcodes lost in each sample
#' @description Draws a bar plot of the number of barcodes with zero reads in
#'     each sample, as computed by \code{\link{barcode_lost}}. A sample that
#'     lost many more barcodes than the others usually has a low quality.
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
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_bar theme geom_text scale_fill_manual
#' @importFrom ggplot2 position_stack
#' @concept plot
#' @return A ggplot object.
#' @seealso \code{\link{barcode_lost}}, \code{\link{plot_barcode_lost_for_gene}}
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' plot_barcode_lost(object)
#' @export
plot_barcode_lost <- function(screenR_Object, palette = NULL,
    alpha = 1, legende_position = "none") {
    object <- update_screenr_object(screenR_Object)
    table <- ScreenR::barcode_lost(object)

    plot <- ggplot(table, aes(
        x = .data$Sample, y = .data$LostBarcode,
        fill = .data$Sample
    )) +
        theme(legend.position = legende_position) +
        geom_bar(alpha = alpha, stat = "identity", color = "black") +
        geom_text(aes(label = .data$LostBarcode),
            position = position_stack(vjust = 0.8), color = "black",
            size = 5
        ) +
        ggplot2::labs(y = "Lost barcodes")

    plot <- plot + palette_scale(object, "Sample", table$Sample, palette)
    return(plot)
}

#' @title Plot the number of barcodes lost for each gene
#' @description Plots, for each gene and sample, the number of barcodes of the
#'     gene with zero reads. Each gene is targeted by several shRNAs
#'     (barcodes), and a gene needs enough barcodes left to give a robust
#'     result: this quality control plot shows which genes lost many of their
#'     barcodes. Only the genes that lost at least one barcode are shown.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param facet Whether to draw a separate panel for each sample (TRUE,
#'     default) or a single panel.
#' @param samples The names of the samples to plot (required).
#' @param palette An optional vector of colors for the samples (named by
#'     sample, or one for each sample in order), which replaces the palette
#'     of the object. By default the Sample palette of the object is used
#'     when it is set (see \code{\link{get_palettes}}), otherwise the
#'     default colors of ggplot2.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr select_if
#' @importFrom tidyselect all_of
#' @importFrom ggplot2 scale_x_continuous
#' @concept plot
#' @return A ggplot object with the number of lost barcodes on the x axis and
#'     the genes on the y axis.
#' @seealso \code{\link{barcode_lost}}, \code{\link{plot_barcode_lost}}
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' plot_barcode_lost_for_gene(object,
#'     samples = c("Time3_A", "Time3_B")
#' )
#' plot_barcode_lost_for_gene(object,
#'     samples = c("Time3_A", "Time3_B"),
#'     facet = FALSE
#' )
#' @export
plot_barcode_lost_for_gene <- function(screenR_Object, facet = TRUE,
    samples, palette = NULL) {
    object <- update_screenr_object(screenR_Object)
    table <- count_mapped_reads(object)
    # The gene of each barcode, from the rowData
    table$Gene <- genes_of(object)[match(table$Barcode, rownames(object))]
    table$Sample <- as.character(table$Sample)
    table <- dplyr::group_by(table, .data$Sample, .data$Gene)

    table <- dplyr::mutate(table, barcode_lost = .data$Mapped == 0)

    table <- dplyr::summarise(table,
        Sample = unique(.data$Sample),
        barcode_lost = sum(.data$barcode_lost), .groups = "drop"
    )
    table <- dplyr::filter(table, .data$barcode_lost != 0)
    table <- tidyr::drop_na(table)
    table <- mutate(table, barcode_lost = as.integer(.data$barcode_lost))

    if (!is.null(samples)) {
        table <- dplyr::filter(table, .data$Sample %in% samples)
    } else {
        stop("The sample vector cannot be NULL.")
    }

    plot <- ggplot(table, aes(.data$barcode_lost, .data$Gene,
        color = .data$Sample
    )) +
        geom_point(show.legend = FALSE) +
        scale_x_continuous(breaks = seq(0, max(table$barcode_lost), 1)) +
        ggplot2::labs(x = "Lost barcodes") +
        palette_scale(object, "Sample", table$Sample, palette,
            aesthetic = "colour")

    if (facet) {
        plot <- plot + ggplot2::facet_wrap(vars(.data$Sample), scales = "free")
    }

    return(plot)
}

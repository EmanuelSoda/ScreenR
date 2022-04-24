#' @title Count Barcode Lost
#' @description This fuction counts the number of Barcode lost during the
#'              sequencing. A barcode is lost if it has zero mapped read.
#'

#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#'
#' @importFrom  dplyr group_by
#' @importFrom  dplyr filter
#' @importFrom  dplyr summarise
#' @importFrom  magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr vars
#' @importFrom tidyr drop_na
#' @concept compute
#' @return Return a tibble containing the number of barcodes lost for each
#'         sample
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' # In order to count the number of barcode lost just the ScreenR object is
#' # needed
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

#' @title Plot number of barcode lost
#' @description This function plots the number of barcodes lost in each sample
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param palette A vector of colors
#' @param alpha A value for the opacity of the plot.
#'              Allowed values are in the range 0 to 1
#' @param legende_position Where to positioning the legend of the plot.
#'                         Allowed values are in the "top", "bottom", "right",
#'                         "left", "none".
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_bar theme geom_text scale_fill_manual
#' @importFrom ggplot2 position_stack
#' @concept plot
#' @return Returns a tibble containing the number of mapped read for sample
#'
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' plot_barcode_lost(object)
#' @export
plot_barcode_lost <- function(screenR_Object, palette = NULL,
    alpha = 1, legende_position = "none") {
    table <- ScreenR::barcode_lost(screenR_Object)

    plot <- ggplot(table, aes(
        x = .data$Sample, y = .data$LostBarcode,
        fill = .data$Sample
    )) +
        geom_bar(stat = "identity", color = "black") +
        theme(legend.position = legende_position) +
        geom_bar(alpha = alpha, stat = "identity", color = "black") +
        geom_text(aes(label = .data$LostBarcode),
            position = position_stack(vjust = 0.8), color = "black",
            size = 5
        )

    if (!is.null(palette)) {
        plot <- plot + scale_fill_manual(values = palette)
    }
    return(plot)
}

#' @title Plot number of barcode lost for gene
#' @description This function plots the number of barcodes lost in each sample
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param facet A boolean to use the facet
#' @param samples The samples to visualize
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr select_if
#' @importFrom tidyselect all_of
#' @concept plot
#' @return Return a tibble containing the number of mapped reads for sample
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' plot_barcode_lost_for_gene(object,
#'     samples = c("Time3_A", "Time3_B")
#' )
#' plot_barcode_lost_for_gene(object,
#'     samples = c("Time3_A", "Time3_B"),
#'     facet = FALSE)
#' @export
plot_barcode_lost_for_gene <- function(screenR_Object, facet = TRUE,
    samples = NULL) {
    numericColumn <- screenR_Object@count_table %>%
        dplyr::select_if(is.numeric) %>%
        colnames()

    table <- tidyr::pivot_longer(
        data = screenR_Object@count_table,
        cols = all_of(numericColumn), names_to = "Sample", values_to = "Mapped"
    )
    table <- dplyr::left_join(table, screenR_Object@annotation_table,
        by = "Barcode"
    )
    table <- dplyr::group_by(table, .data$Sample, .data$Gene)

    table <- dplyr::mutate(table, barcode_lost = .data$Mapped == 0)

    table <- dplyr::summarise(table,
        Sample = unique(.data$Sample),
        barcode_lost = sum(.data$barcode_lost), .groups = "drop"
    )
    table <- dplyr::filter(table, .data$barcode_lost != 0)
    table <- tidyr::drop_na(table)

    if (!is.null(samples)) {
        table <- dplyr::filter(table, .data$Sample %in% samples)
    }

    plot <- ggplot(table, aes(.data$barcode_lost, .data$Gene,
        fill = .data$Sample
    )) +
        geom_bar(
            stat = "identity",
            position = ggplot2::position_dodge(), show.legend = FALSE
        )

    if (facet) {
        plot <- plot + ggplot2::facet_wrap(vars(.data$Sample), scales = "free")
    }

    return(plot)
}


#' #' @title Plot distribution of barcode lost
#' #' @description The function plots the distribution of the lost barcodes in
#' #'              each sample
#' #' @param screenR_Object The ScreenR object obtained using the
#' #'                       \code{\link{create_screenr_object}}
#' #' @param palette A vector of colors
#' #' @param alpha  A value for the opacity of the plot.
#' #'              Allowed values are in the range 0 to 1
#' #' @param type Type of plot. Allowed values are "boxplot" and "boxplot"
#' #' @importFrom magrittr %>%
#' #' @importFrom rlang .data
#' #' @importFrom ggplot2 position_dodge scale_fill_manual geom_boxplot
#' #' @importFrom ggplot2 geom_density
#' #' @concept plot
#' #' @return Return a tibble containing the number of mapped read for sample
#' #' @examples
#' #' # object <- get0('object', envir = asNamespace('ScreenR'))
#' #' # plot_distribution_of_barcode_lost(object)
#' #' # plot_distribution_of_barcode_lost(object, type = 'density')
#' #'
#' plot_distribution_of_barcode_lost <- function(screenR_Object,
#'     palette = NULL, alpha = 1, type = "boxplot") {
#'     table <- ScreenR::barcode_lost(screenR_Object)
#'
#'     if (toupper(type) == toupper("boxplot")) {
#'         plot <- ggplot(data = table, aes(
#'             x = .data$Sample,
#'             y = .data$LostBarcode, fill = .data$Sample
#'         )) +
#'             ggplot2::geom_boxplot(alpha = alpha)
#'     } else if (toupper(type) == toupper("density")) {
#'         plot <- ggplot(data = table, aes(
#'             x = .data$LostBarcode,
#'             fill = .data$Sample
#'         )) +
#'             ggplot2::geom_density(alpha = alpha)
#'     } else {
#'         warning("You have selected:\nPlease select the right type")
#'     }
#'
#'     if (!is.null(palette)) {
#'         plot <- plot + ggplot2::scale_fill_manual(values = palette)
#'     } else {
#'         warning("The palette is null")
#'     }
#'
#'     return(plot)
#' }

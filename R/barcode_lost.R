#' @title Count Barcode Lost
#' @description This fuction count the number of Barcode lost during the
#'              sequenceing. A barcode is lost if it has zero mapped read
#'
#'
#'
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#'
#' @import dplyr
#' @importFrom  magrittr %>%
#' @importFrom rlang .data
#' @return return a tibble containing the number of barcode lost for sample
#' @export

barcode_lost <- function(screenR_Object) {
    table <- count_mapped_reads(screenR_Object)
    table <- table %>%
        dplyr::group_by(.data$Sample) %>%
        dplyr::filter(.data$Mapped == 0) %>%
        dplyr::summarise(LostBarcode = n())

    return(table)
}

#' @title Plot number of barcode lost
#' @description This function plots the number of barcode lost in each sample
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @param palette A vector of colors
#' @param alpha A value for the opacity of the plot
#' @param legende_position Where to positioning the legend of the plot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @concept plot
#' @return return a tibble containing the number of mapped read for sample
#' @export
plot_barcode_lost <- function(screenR_Object, palette = NULL, alpha = 1,
    legende_position = "none") {
    table <- barcode_lost(screenR_Object)

    plot <- ggplot(table, aes(x = .data$Sample, y = .data$LostBarcode,
        fill = .data$Sample)) +
        geom_bar(stat = "identity", color = "black") +
        theme(legend.position = legende_position) +
        geom_bar(alpha = alpha,
        stat = "identity", color = "black") +
        geom_text(aes(label = .data$LostBarcode),
        position = position_stack(vjust = 0.8), color = "black", size = 5)

    if (!is.null(palette)) {
        plot <- plot + scale_fill_manual(values = palette)

    }
    return(plot)
}

#' @title Plot number of barcode lost for gene
#' @description This function plots the number of barcode lost in each sample
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @concept plot
#' @return return a tibble containing the number of mapped read for sample
#' @export
plot_barcode_lost_for_gene <- function(screenR_Object) {
    numericColumn <- screenR_Object@count_table %>%
        dplyr::select_if(is.numeric) %>%
        colnames()

    table <- screenR_Object@count_table %>%
        tidyr::gather(.data$Sample, .data$Mapped, all_of(numericColumn)) %>%
        dplyr::mutate(Sample = factor(.data$Sample, levels = numericColumn))

    table %>%
        dplyr::left_join(screenR_Object@annotation_table, by = .data$Barcode) %>%
        dplyr::group_by(.data$Sample, .data$Gene) %>%
        dplyr::mutate(barcode_lost = .data$Mapped == 0) %>%
        dplyr::summarise(Sample = unique(.data$Sample),
                         barcode_lost = sum(.data$barcode_lost)) %>%
        dplyr::filter(.data$barcode_lost != 0) %>%
        tidyr::drop_na() %>%
        ggplot(aes(.data$Gene, .data$barcode_lost, fill = .data$Sample)) +
        geom_bar(stat = "identity", position = position_dodge())
    theme(axis.ticks = element_line(size = 0.3), legend.position = "none",
        legend.direction = "horizontal", axis.text.x = element_text(angle = 40,
            hjust = 1)) + facet_grid(rows = vars(.data$Sample))
}


#' @title Plot distribution of barcode lost
#' @description The function plot the distribution of the lost barcode in each
#'              sample
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @param palette A vector of colors
#' @param alpha A value for the opacity of the plot
#' @param type Type of plot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @concept plot
#' @return return a tibble containing the number of mapped read for sample
#' @export

plot_distribution_of_barcode_lost <- function(screenR_Object, palette = NULL,
                                              alpha = 1, type = "boxplot") {
    table <- barcode_lost(screenR_Object)

    if (toupper(type) == toupper("boxplot")) {
        plot <- ggplot(data = table, aes(x = .data$Sample,
                                         y = .data$LostBarcode,
                                         fill = .data$Sample)) +
            geom_boxplot(alpha = alpha)

    } else if (toupper(type) == toupper("density")) {
        plot <- ggplot(data = table, aes(x = .data$LostBarcode,
                                         fill = .data$Sample)) +
            geom_density(alpha = alpha)
    } else {
            warning(paste(paste("You have selected:\n", type, sep = ""),
                          "Please select the right type", sep = "\n"))
        }

    if (!is.null(palette)) {
        plot <- plot + scale_fill_manual(values = palette)
    }  else {
        warning("The palette is null")
    }

    return(plot)
}



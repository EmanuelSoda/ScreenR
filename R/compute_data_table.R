#' @title Create the ScreenR Object
#' @description Initial function to create the Screen Object.
#'
#' @param screenR_Object The ScreenR object
#'
#' @return screenR_Object
#' @export
#' @importFrom rlang .data
#' @importFrom stringr str_split_fixed
#' @examples
#' \dontrun{
#' compute_data_table(object)
#' }
#'
compute_data_table <- function(screenR_Object) {
    # First the table is created with the join of the annotation
    # and the count table
    data <- screenR_Object@normalized_count_table
    table <- data %>%
        tidyr::gather("Sample", "Frequency", colnames(data)[2]:last(colnames(data))) %>%
        dplyr::mutate(Barcode = as.factor(.data$Barcode)) %>%
        dplyr::left_join(screenR_Object@annotation_table, by = "Barcode") %>%
        select(.data$Barcode, .data$Gene, .data$Sample, .data$Frequency,
            .data$Sequence, .data$Library, .data$Gene_ID) %>%
        mutate(Day = str_split_fixed(.data$Sample, pattern = "_", n = 3)[,
            1], Treatment = gsub(".*_", "", gsub("(.*)_\\w+", "\\1",
            .data$Sample)))

    table <- dplyr::mutate(table, Sample = factor(x = .data$Sample,
        levels = unique(.data$Sample)))

    # Then the table is put into the object
    screenR_Object@data_table <- table
    return(screenR_Object)
}

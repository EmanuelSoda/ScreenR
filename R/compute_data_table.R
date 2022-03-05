#' @title Compute data Table
#' @description This function computes the data table that will be used
#'              for the analysis.
#'
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#'
#' @return ScreenR_Object with the data_table filed containing the table.
#' @export
#' @importFrom rlang .data
#' @importFrom stringr str_split_fixed
#' @examples
#' obj <- get0('obj', envir = asNamespace('ScreenR'))
#'
#' compute_data_table(obj)

#'
compute_data_table <- function(screenR_Object) {
    # First the table is created with the join of the annotation and the count
    # table
    data <- screenR_Object@normalized_count_table
    table <- data %>%
        tidyr::pivot_longer(!.data$Barcode, names_to = "Sample",
            values_to = "Frequency") %>%
        dplyr::mutate(Barcode = as.factor(.data$Barcode)) %>%
        dplyr::left_join(screenR_Object@annotation_table, by = "Barcode") %>%
        select(.data$Barcode, .data$Gene, .data$Sample, .data$Frequency,
            .data$Sequence, .data$Library, .data$Gene_ID) %>%
        mutate(Day = str_split_fixed(.data$Sample, pattern = "_",
            n = 3)[, 1], Treatment = gsub(".*_", "", gsub("(.*)_\\w+",
            "\\1", .data$Sample)))

    table <- dplyr::mutate(table, Sample = factor(x = .data$Sample,
        levels = unique(.data$Sample)))

    # Then the table is put into the object
    screenR_Object@data_table <- table
    return(screenR_Object)
}

#' @title Compute data Table
#' @description This function computes the data table that will be used
#'              for the analysis. The data_table is a tidy and normalized
#'              version of the original count_table and will be used
#'              throughout the analysis. The Day and Treatment columns come
#'              from \code{\link{get_sample_table}}.
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#'
#' @return ScreenR_Object with the data_table filed containing the table.
#' @export
#' @importFrom rlang .data
#' @importFrom  tidyr pivot_longer
#' @importFrom  dplyr left_join
#' @concept compute
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' object <- compute_data_table(object)
#' head(slot(object, "data_table"))
compute_data_table <- function(screenR_Object) {
    # First the table is created with the join of the annotation and the count
    # table
    screenR_Object <- update_screenr_object(screenR_Object)
    data <- normalized_table_of(screenR_Object)
    # The Day and Treatment of each sample, given by the user or taken from
    # the sample names
    sample_table <- get_sample_table(screenR_Object)
    sample_table <- sample_table[, c("Sample", "Day", "Treatment")]
    table <- data %>%
        tidyr::pivot_longer(!.data$Barcode,
            names_to = "Sample",
            values_to = "Frequency"
        ) %>%
        dplyr::mutate(Barcode = as.factor(.data$Barcode)) %>%
        dplyr::left_join(annotation_of(screenR_Object), by = "Barcode") %>%
        # Sequence, Library and Gene_ID are kept when the annotation has them
        dplyr::select(
            "Barcode", "Gene", "Sample", "Frequency",
            dplyr::any_of(c("Sequence", "Library", "Gene_ID"))
        ) %>%
        dplyr::left_join(sample_table, by = "Sample")

    table <- dplyr::mutate(table, Sample = factor(
        x = .data$Sample,
        levels = unique(.data$Sample)
    ))

    # Then the table is put into the object
    screenR_Object@data_table <- table
    return(screenR_Object)
}

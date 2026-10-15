#' @title Compute the data table
#' @description Computes the data table, a tidy (long) version of the
#'     normalized count table used by many functions of the analysis, such as
#'     \code{\link{compute_metrics}}, \code{\link{plot_boxplot}},
#'     \code{\link{plot_trend}} and \code{\link{filter_by_slope}}. Run it
#'     after \code{\link{normalize_data}}. The Day and Treatment of each
#'     sample come from \code{\link{get_sample_table}}.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @return The ScreenR object with the data table filled. The data table has
#'     one row for each barcode and sample and the columns Barcode, Gene,
#'     Sample, Frequency (the normalized count), Day and Treatment, plus
#'     Sequence, Library and Gene_ID when the annotation table has them. It
#'     can be read with \code{\link{get_data_table}}.
#' @export
#' @importFrom rlang .data
#' @importFrom  tidyr pivot_longer
#' @importFrom  dplyr left_join
#' @concept compute
#' @seealso \code{\link{normalize_data}}, \code{\link{get_data_table}}
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' object <- normalize_data(object)
#' object <- compute_data_table(object)
#' head(get_data_table(object))
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
        tidyr::pivot_longer(!"Barcode",
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

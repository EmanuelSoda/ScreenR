#' @title Compute the data table
#' @description Checks that the object is normalized and returns it. The data
#'     table, the tidy (long) version of the normalized counts used by
#'     \code{\link{compute_metrics}}, \code{\link{plot_boxplot}},
#'     \code{\link{plot_trend}}, \code{\link{filter_by_slope}} and other
#'     functions, is no longer stored in the object: it is computed from the
#'     object when needed (see \code{\link{get_data_table}}), so it always
#'     follows the counts, the annotation, the sample table and the
#'     normalization. This function is kept so that existing code works; it
#'     is not needed any more.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}, normalized with
#'     \code{\link{normalize_data}}.
#' @return The ScreenR object. It is an error if it is not normalized.
#' @export
#' @concept compute
#' @seealso \code{\link{normalize_data}}, \code{\link{get_data_table}}
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' object <- normalize_data(object)
#' object <- compute_data_table(object)
#' head(get_data_table(object))
compute_data_table <- function(screenR_Object) {
    object <- update_screenr_object(screenR_Object)
    effective_lib_size(object, "compute_data_table()")
    object
}

#' @title Build the Data Table
#' @description Internal function that builds the data table of a normalized
#'              object: one row for each barcode and sample, with the columns
#'              Barcode, Gene, Sample, Frequency (the normalized count, raw
#'              count / effective library size * 1e6), the Sequence, Library
#'              and Gene_ID columns of the annotation when present, Day and
#'              Treatment. It is computed from the assays, rowData and
#'              colData each time and never stored.
#' @param object A ScreenR object of the current layout.
#' @param what What needs the data table, for the error message.
#' @importFrom rlang .data
#' @return A tibble.
#' @keywords internal
data_table_of <- function(object, what = "This function") {
    values <- normalized_counts(object, what)
    n_barcodes <- nrow(values)
    n_samples <- ncol(values)
    identifiers <- rownames(object)
    if (is.null(identifiers)) identifiers <- character()
    samples <- colnames(object)
    row_data <- SummarizedExperiment::rowData(object)
    sample_table <- sample_table_of(object)
    sample_rows <- match(samples, sample_table$Sample)

    # One row for each barcode and sample, barcode by barcode
    barcode_index <- rep(seq_len(n_barcodes), each = n_samples)
    sample_index <- rep(seq_len(n_samples), times = n_barcodes)
    table <- tibble::tibble(
        Barcode = factor(identifiers[barcode_index], levels = identifiers),
        Gene = genes_of(object)[barcode_index],
        Sample = factor(samples[sample_index], levels = samples),
        Frequency = as.vector(t(values))
    )
    for (column in intersect(c("Sequence", "Library", "Gene_ID"),
        colnames(row_data))) {
        table[[column]] <- row_data[[column]][barcode_index]
    }
    table$Day <- as.character(sample_table$Day)[sample_rows][sample_index]
    table$Treatment <-
        as.character(sample_table$Treatment)[sample_rows][sample_index]
    table
}

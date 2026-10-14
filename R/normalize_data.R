#' @title Normalize data
#' @description This function normalizes the counts to Counts Per Million
#'              (CPM), so that samples sequenced at different depths can be
#'              compared. Three methods are available:
#'              \itemize{
#'                \item "cpm" (default): each count is divided by the total
#'                      number of reads of its sample and multiplied by 1e6.
#'                \item "tmm": as "cpm", but the library sizes are corrected
#'                      with the trimmed mean of M-values (TMM) normalization
#'                      factors of edgeR (\code{\link[edgeR]{normLibSizes}}).
#'                      This is more robust when strong hits deplete a large
#'                      part of the library.
#'                \item "control": each count is divided by the number of
#'                      reads of the control barcodes (for example
#'                      non-targeting shRNAs) of its sample and multiplied by
#'                      1e6. This assumes that the control barcodes are not
#'                      affected by the treatment.
#'              }
#'              The normalization only affects the normalized count table
#'              (and so the data table, the Z-score and the plots). The
#'              edgeR based methods (CAMERA, ROAST) always model the raw
#'              counts.
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param method The normalization method: "cpm" (default), "tmm" or
#'               "control".
#' @param control_barcodes The barcodes to use for the "control" method.
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom purrr map_if
#' @importFrom edgeR DGEList normLibSizes
#' @return Return the ScreenR object with the normalize data
#' @export
#' @concept  compute
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' object <- normalize_data(object)
#'
#' get_normalized_count_table(object)
#'
#' # TMM normalization
#' object <- normalize_data(object, method = "tmm")
normalize_data <- function(screenR_Object, method = c("cpm", "tmm", "control"),
    control_barcodes = NULL) {
    method <- match.arg(method)
    screenR_Object <- update_screenr_object(screenR_Object)
    count_table <- count_table_of(screenR_Object)
    numeric_columns <- vapply(count_table, is.numeric, logical(1))
    counts <- as.matrix(count_table[, numeric_columns])

    if (method == "cpm") {
        library_size <- colSums(counts)
    } else if (method == "tmm") {
        dge <- edgeR::normLibSizes(edgeR::DGEList(counts = counts))
        library_size <- dge$samples$lib.size * dge$samples$norm.factors
    } else {
        if (is.null(control_barcodes)) {
            stop("control_barcodes must be given when method = 'control'.")
        }
        is_control <- as.character(count_table$Barcode) %in%
            as.character(control_barcodes)
        if (!any(is_control)) {
            stop("None of the control_barcodes is in the count table.")
        }
        library_size <- colSums(counts[is_control, , drop = FALSE])
        if (any(library_size == 0)) {
            stop("The control barcodes have zero reads in some samples.")
        }
    }

    normalized <- count_table
    normalized[, numeric_columns] <- sweep(counts, 2, library_size, "/") *
        1e+06
    screenR_Object <- set_normalized_count_table(screenR_Object,
        dplyr::as_tibble(normalized))

    return(screenR_Object)
}

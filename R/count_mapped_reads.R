#' @title Get the mapped reads of each barcode in each sample
#' @description Returns the count table in long format, with the number of
#'     reads mapped to each barcode in each sample. It is the starting point
#'     of the quality control functions, such as \code{\link{mapped_reads}},
#'     \code{\link{barcode_lost}} and
#'     \code{\link{plot_mapped_reads_distribution}}.
#' @importFrom rlang .data
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @return A tibble with one row for each barcode and sample and the columns
#'     Barcode, Sample (a factor in the order of the samples of the count
#'     table) and Mapped (the number of reads).
#' @seealso \code{\link{mapped_reads}}, \code{\link{barcode_lost}},
#'     \code{\link{plot_mapped_reads_distribution}}
#' @export
#' @concept compute
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' head(count_mapped_reads(object))
count_mapped_reads <- function(screenR_Object) {
    # Raw-count quality control, from the counts assay
    object <- update_screenr_object(screenR_Object)
    counts <- SummarizedExperiment::assay(object, "counts",
        withDimnames = TRUE)
    samples <- colnames(object)
    identifiers <- rownames(object)
    if (is.null(identifiers)) identifiers <- character()
    tibble::tibble(
        Barcode = rep(identifiers, times = length(samples)),
        Sample = factor(rep(samples, each = nrow(object)), levels = samples),
        Mapped = as.vector(counts)
    )
}

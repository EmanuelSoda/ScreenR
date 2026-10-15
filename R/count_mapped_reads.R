#' @title Get the mapped reads of each barcode in each sample
#' @description Returns the count table in long format, with the number of
#'     reads mapped to each barcode in each sample. It is the starting point
#'     of the quality control functions, such as \code{\link{mapped_reads}},
#'     \code{\link{barcode_lost}} and
#'     \code{\link{plot_mapped_reads_distribution}}.
#' @importFrom rlang .data
#' @importFrom tidyr gather
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
    # Get only the numeric column (so the sample)
    count_table <- count_table_of(screenR_Object)
    numericColumn <- count_table %>%
        dplyr::select_if(is.numeric) %>%
        colnames()

    table <- count_table %>%
        tidyr::gather("Sample", "Mapped", all_of(numericColumn)) %>%
        dplyr::mutate(Sample = factor(.data$Sample, levels = numericColumn))
    return(table)
}

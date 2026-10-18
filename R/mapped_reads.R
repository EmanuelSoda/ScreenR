#' @title Compute the number of mapped reads of each sample
#' @description Computes the total number of reads mapped to the barcodes in
#'     each sample (the library size). Samples with far fewer mapped reads
#'     than the others may have a lower quality. Use
#'     \code{\link{plot_mapped_reads}} to plot the result.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @importFrom rlang .data
#' @importFrom dplyr select
#' @return A tibble with one row for each sample and the columns Sample and
#'     Mapped (the total number of mapped reads).
#' @seealso \code{\link{count_mapped_reads}}, \code{\link{plot_mapped_reads}},
#'     \code{\link{barcode_lost}}
#' @concept compute
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' mapped_reads(object)
mapped_reads <- function(screenR_Object) {
    table <- count_mapped_reads(screenR_Object)

    table <- table %>%
        dplyr::select("Sample", "Mapped") %>%
        dplyr::group_by(.data$Sample) %>%
        dplyr::mutate(Mapped = sum(.data$Mapped)) %>%
        summarise(Sample = unique(.data$Sample), Mapped = unique(.data$Mapped))

    return(table)
}

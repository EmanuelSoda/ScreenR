#' @title Find hits with the robust Z-score threshold rule
#' @description Finds the genes with more than number_barcode barcodes whose
#'     robust Z-score is below the median of the robust Z-scores of all the
#'     barcodes. It is the same rule as \code{\link{find_zscore_hit}}, applied
#'     to the ZscoreRobust column; like that rule it gives no p-value: use
#'     \code{\link{find_gene_zscore_hit}} with score = "ZscoreRobust" for a
#'     gene-level test with an FDR.
#' @param table_treate_vs_control The table of barcode metrics returned by
#'     \code{\link{compute_metrics}}, with one row for each barcode and the
#'     columns Gene and ZscoreRobust.
#' @param number_barcode A gene is a hit when more than number_barcode of its
#'     barcodes have a robust Z-score below the median (default 6).
#' @importFrom dplyr n
#' @return An ungrouped tibble with the columns Gene and numberOfBarcode (the
#'     number of barcodes of the gene below the median), with one row for each
#'     hit.
#' @seealso \code{\link{find_zscore_hit}}, \code{\link{find_gene_zscore_hit}},
#'     \code{\link{compute_metrics}}
#' @export
#' @concept find
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' table <- compute_metrics(object,
#'     control = "Time3", treatment = "TRT",
#'     day = "Time3"
#' )
#' result <- find_robust_zscore_hit(table, number_barcode = 6)
#' head(result)
find_robust_zscore_hit <- function(table_treate_vs_control,
    number_barcode = 6) {
    hit_table <- table_treate_vs_control %>%
        dplyr::filter(.data$ZscoreRobust < median(.data$ZscoreRobust)) %>%
        dplyr::group_by(.data$Gene) %>%
        dplyr::summarise(numberOfBarcode = n()) %>%
        dplyr::filter(.data$numberOfBarcode > number_barcode)

    return(hit_table)
}

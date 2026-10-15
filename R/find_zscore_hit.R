#' @title Find hits with the Z-score threshold rule
#' @description Finds the genes with more than number_barcode barcodes whose
#'     Z-score is below (direction = "Down", the default) or above
#'     (direction = "Up") the median (or the mean) of the Z-scores of all the
#'     barcodes. This simple rule gives no p-value and flags many genes
#'     without any effect (with the median, about 17\% of them when genes
#'     have 10 barcodes and number_barcode = 6): use \code{\link{find_gene_zscore_hit}}, which
#'     computes a p-value and an FDR for each gene, instead.
#' @param table_treate_vs_control The table of barcode metrics returned by
#'     \code{\link{compute_metrics}}, with one row for each barcode and the
#'     columns Gene and Zscore.
#' @param number_barcode A gene is a hit when more than number_barcode of its
#'     barcodes have a Z-score below the threshold (default 6). For example,
#'     with the default a gene with 10 barcodes is a hit when at least 7 of
#'     them are below the threshold.
#' @param metric The threshold on the Z-score: "median" (the default) or
#'     "mean" of the Z-scores of all the barcodes.
#' @param direction "Down" (default) to find depleted genes (barcodes below
#'     the threshold) or "Up" to find enriched genes (barcodes above it).
#' @return A tibble with the columns Gene and numberOfBarcode (the number of
#'     barcodes of the gene below, or above, the threshold), with one row for
#'     each hit.
#' @seealso \code{\link{find_gene_zscore_hit}},
#'     \code{\link{find_robust_zscore_hit}}, \code{\link{compute_metrics}},
#'     \code{\link{find_common_hit}}
#' @export
#' @concept find
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' table <- compute_metrics(object,
#'     control = "Time3", treatment = "TRT",
#'     day = "Time3"
#' )
#'
#' # Threshold on the median
#' result <- find_zscore_hit(table, number_barcode = 6)
#' head(result)
#'
#' # Threshold on the mean
#' result <- find_zscore_hit(table, number_barcode = 6, metric = "mean")
#' head(result)
find_zscore_hit <- function(table_treate_vs_control, number_barcode = 6,
    metric = c("median", "mean"), direction = c("Down", "Up")) {
    metric <- match.arg(metric)
    direction <- match.arg(direction)
    zscore <- table_treate_vs_control$Zscore
    threshold <- if (metric == "median") median(zscore) else mean(zscore)
    # Keep the barcodes on the requested side of the threshold, then the genes
    # with more than number_barcode such barcodes
    keep <- if (direction == "Down") zscore < threshold else zscore > threshold
    hit_table <- table_treate_vs_control[keep, ] %>%
        dplyr::group_by(.data$Gene) %>%
        dplyr::reframe(numberOfBarcode = n()) %>%
        dplyr::filter(.data$numberOfBarcode > number_barcode)
    hit_table <- dplyr::ungroup(hit_table)
    return(hit_table)
}

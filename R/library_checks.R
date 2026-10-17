#' @title Count the barcodes of each gene
#' @description Counts the barcodes (shRNAs or sgRNAs) of each gene among
#'     the barcodes of a ScreenR object (the rows of the counts, with the gene
#'     given by the rowData), which are the barcodes used by the analyses.
#'     Annotated barcodes that are not in the count table are not counted.
#'     Use it to choose \code{number_barcode}
#'     (in \code{\link{find_camera_hit}}, \code{\link{find_roast_hit}},
#'     \code{\link{find_time_hit}} and \code{\link{plot_barcode_hit}}) and
#'     \code{min_barcodes} (in \code{\link{find_gene_zscore_hit}}): shRNA
#'     libraries often have 5 to 25 barcodes per gene, CRISPR libraries 3
#'     to 6 sgRNAs per gene (4 in Brunello and TKOv3).
#' @param screenR_Object A ScreenR object created with
#'     \code{\link{create_screenr_object}}.
#' @return A tibble with the columns Gene and NBarcodes, ordered by gene.
#' @seealso \code{\link{create_screenr_object}}
#' @concept objects
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' per_gene <- barcodes_per_gene(object)
#' head(per_gene)
#' table(per_gene$NBarcodes)
barcodes_per_gene <- function(screenR_Object) {
    # The barcodes of the object (the ones the analyses use), from the
    # rowData
    genes <- genes_of(update_screenr_object(screenR_Object))
    genes <- genes[!is.na(genes)]
    counts <- table(genes)
    tibble::tibble(Gene = names(counts), NBarcodes = as.integer(counts))
}

#' @title Check a Threshold on the Number of Barcodes
#' @description Warns when a threshold on the number of barcodes per gene
#'     excludes more than max_excluded of the genes, for example
#'     number_barcode = 3 (genes need more than 3 barcodes) with a CRISPR
#'     library of 4 sgRNAs per gene in which some sgRNAs were filtered out.
#'     The warning reports the median number of barcodes per gene and
#'     suggests a threshold that keeps most of the genes.
#' @param n_barcodes The number of barcodes of each gene.
#' @param threshold The threshold.
#' @param strict TRUE when genes need more than threshold barcodes
#'     (number_barcode), FALSE when they need at least threshold barcodes
#'     (min_barcodes).
#' @param argument The name of the argument, used in the message.
#' @param max_excluded The fraction of excluded genes above which a warning
#'     is given.
#' @return Invisibly, the fraction of excluded genes.
#' @keywords internal
check_barcode_threshold <- function(n_barcodes, threshold, strict = TRUE,
    argument = "number_barcode", max_excluded = 0.2) {
    n_barcodes <- as.numeric(n_barcodes)
    if (length(n_barcodes) == 0) return(invisible(0))
    excluded <- if (strict) n_barcodes <= threshold else n_barcodes < threshold
    fraction <- mean(excluded)
    if (fraction > max_excluded) {
        # The largest threshold that keeps at least 1 - max_excluded of the
        # genes
        quantile_low <- stats::quantile(n_barcodes, max_excluded,
            type = 1, names = FALSE)
        suggestion <- if (strict) {
            max(quantile_low - 1, 0)
        } else {
            max(quantile_low, 1)
        }
        warning(sprintf(paste0(
            "%.0f%% of the genes have %s %g barcodes and are not tested ",
            "(%s = %g). The library has a median of %g barcodes per gene: ",
            "consider %s = %g (see barcodes_per_gene())."),
            100 * fraction, if (strict) "at most" else "fewer than",
            threshold, argument, threshold, stats::median(n_barcodes),
            argument, suggestion), call. = FALSE)
    }
    invisible(fraction)
}

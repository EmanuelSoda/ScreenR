#' @title Plot the barcodes of a gene (barcode plot)
#' @description Draws a \code{\link[limma]{barcodeplot}} for a gene: all the
#'     barcodes are ranked by their log2 fold change (computed with edgeR for
#'     the given contrast) and the barcodes of the gene are shown as vertical
#'     bars. If most of the bars are on the left (negative log2 fold change)
#'     the gene is depleted, if they are on the right it is enriched. It is
#'     useful to check the hits of \code{\link{find_camera_hit}} and
#'     \code{\link{find_roast_hit}}.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param matrix_model The design matrix of the linear model, for example
#'     created with \code{\link{create_design_matrix}} (or with
#'     \code{\link[stats]{model.matrix}}).
#' @param contrast The comparison to plot: the name or index of a column of
#'     matrix_model, a numeric contrast vector (for example created with
#'     \code{\link{create_contrast}}) or a one-column matrix created with
#'     \code{\link[limma]{makeContrasts}}.
#' @param number_barcode The gene must have more than number_barcode barcodes
#'     (default 3), as in \code{\link{find_camera_hit}}.
#' @param quantile The two log2 fold change values that delimit the central
#'     (shaded) region of the plot. Default c(-0.5, 0.5).
#' @param labels The labels of the left and right ends of the plot.
#' @param gene The name of the gene to plot.
#' @concept  plot
#' @return Draws the plot with base graphics and returns NULL invisibly.
#' @seealso \code{\link{find_camera_hit}}, \code{\link{find_roast_hit}},
#'     \code{\link{plot_barcode_trend}}
#' @importFrom edgeR estimateDisp
#' @importFrom edgeR glmFit
#' @importFrom edgeR glmLRT
#' @importFrom limma barcodeplot
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' matrix_model <- create_design_matrix(object)
#' contrast <- create_contrast(matrix_model,
#'     treatment = "Treated", control = "Control"
#' )
#'
#' plot_barcode_hit(object, matrix_model,
#'     contrast = contrast,
#'     gene = "Gene_300"
#' )
plot_barcode_hit <- function(screenR_Object, matrix_model,
    contrast, number_barcode = 3, gene,
    quantile = c(-0.5, 0.5), labels = c("Negative logFC", "Positive logFC")) {
    contrast <- as_contrast(contrast, matrix_model)
    DGEList <- create_edger_obj(screenR_Object)
    xglm <- edgeR::estimateDisp(DGEList, design = matrix_model)
    log2fc <- barcode_log2fc(xglm, matrix_model, contrast)

    genesymbollist <- unique_gene_symbols(DGEList$genes$Gene, number_barcode)
    if (is.null(genesymbollist[[gene]])) {
        stop("Gene ", gene, " is not in the annotation or does not have ",
            "more than number_barcode = ", number_barcode, " barcodes.")
    }

    plot <- limma::barcodeplot(log2fc,
        index = genesymbollist[[gene]],
        main = paste("Barcode plot for Gene", gene), labels = labels,
        quantile = quantile
    )

    return(plot)
}

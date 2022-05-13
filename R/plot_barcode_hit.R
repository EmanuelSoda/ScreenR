#' @title Plot barcode hit
#' @description Create a barcode plot for a hit.
#'              A barcode plot displays if the hit is differentially up or
#'              down regulated. If most of the vertical line are on the left
#'              side the gene associated to the barcodes is down regulated
#'              otherwise is up regulated.
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param matrix_model The matrix that will be used to perform the
#'                     linear model analysis. It is created using
#'                     model.matrix.

#' @param contrast An object created with \code{\link[limma]{makeContrasts}}
#'                 function.
#' @param number_barcode Number of barcode that as to be differentially
#'                       expressed (DE) in order to consider the associated gene
#'                       DE. Example a gene is associated with 10 shRNA we
#'                       consider a gene DE if it has at least
#'                       number_barcode = 5 shRNA DE.
#' @param quantile Quantile to display on the plot
#' @param labels The label to be displayed on the quantile side
#' @param gene The name of the gene that has to be plot
#' @concept  plot
#' @return The barcode plot
#' @importFrom edgeR estimateDisp
#' @importFrom edgeR glmFit
#' @importFrom edgeR glmLRT
#' @importFrom limma barcodeplot
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' matrix_model <- model.matrix(~ slot(object, "groups"))
#' colnames(matrix_model) <- c("Control", "T1_T2", "Treated")
#' contrast <- limma::makeContrasts(Treated - Control, levels = matrix_model)
#'
#' plot_barcode_hit(object, matrix_model,
#'     contrast = contrast,
#'     gene = "Gene_300"
#' )
plot_barcode_hit <- function(screenR_Object, matrix_model,
    contrast, number_barcode = 3, gene,
    quantile = c(-0.5, 0.5), labels = c("Negative logFC", "Positive logFC")) {
    DGEList <- create_edger_obj(screenR_Object)
    xglm <- edgeR::estimateDisp(DGEList, coef = seq(
        1,
        length(colnames(matrix_model)), 1
    ))
    fit <- edgeR::glmFit(xglm, matrix_model)

    lrt <- edgeR::glmLRT(fit, contrast = contrast)


    genesymbols <- as.character(DGEList$genes[, 1])
    genesymbollist <- list()
    unq <- unique(genesymbols)
    unq <- unq[!is.na(unq)]
    for (i in unq) {
        sel <- genesymbols == i & !is.na(genesymbols)
        if (sum(sel) > 3) {
            genesymbollist[[i]] <- which(sel)
        }
    }

    plot <- limma::barcodeplot(lrt$table$logFC,
        index = genesymbollist[[gene]],
        main = paste("Barcode plot for Gene", gene), labels = labels,
        quantile = quantile
    )

    return(plot)
}

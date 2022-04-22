#' @title Plot Barcode Hit
#' @description Create a Barcode plot for the Hit
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @param matrix_model the matrix that will be used to perform the
#'                     linear model analysis

#' @param contrast An object created with \code{\link[limma]{makeContrasts}}
#'                 function.
#' @param number_barcode Number of barcode to have under the median
#' @param quantile Quantile to display on the plot
#' @param labels Title of the plot
#' @param gene A  gene name to plot
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
#' plot_barcode_hit(object, matrix_model, contrast = contrast,
#'                  gene = "Gene_300")
#'
plot_barcode_hit <- function(screenR_Object, matrix_model,
    contrast, number_barcode = 3, gene, quantile = c(
        -0.5,
        0.5
    ), labels = c("Negative logFC", "Positive logFC")) {
    DGEList <- create_edgeR_obj(screenR_Object)
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

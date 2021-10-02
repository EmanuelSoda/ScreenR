#' @title Plot Barcode Hit
#' @description Create a Barcode plot for the Hit
#' @param screenR_Object The Object of the package
#' @param matrix_model the matrix that will be used to perform the
#'                     linear model analysis
#' @param hit_common The vector of the common hit
#' @param contrast An object created with \code{\link{limma::makeContrasts}}
#'                 function
#' @param number_barcode Number of barcode to have under the median
#' @param number_plot_row Number of row for the plot
#' @param number_plot_col Number of col for the plot
#' @param quantile Quantile to diplay on the plot
#' @param labels Title of the plot
#'
#' @return A vector containing the common hit
#' @export
plot_barcode_hit <- function(screenR_Object, matrix_model, hit_common,
                             contrast,
                             number_barcode = 3,
                             number_plot_row = 3,
                             number_plot_col = 3,
                             gene,
                             quantile = c(-0.5, 0.5),
                             labels = c("Negative logFC", "Positive logFC")){

  DGEList <- create_edgeR_obj(screenR_Object)
  xglm <- edgeR::estimateDisp(DGEList, matrix_model)
  fit <- edgeR::glmFit(xglm, matrix_model)


  lrt <- edgeR::glmLRT(fit, contrast = contrast)


  genesymbols <- DGEList$genes[, 1]
  genesymbollist <- unique_gene_symbols(genesymbols, number_barcode)

  par(mfrow=c(number_plot_row, number_plot_col))
  limma::barcodeplot(lrt$table$logFC, index=genesymbollist[[gene]],
                     main= paste("Barcode plot for Gene", gene),
                     labels=labels,
                     quantile=quantile)

}


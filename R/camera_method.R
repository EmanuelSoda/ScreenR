#' @title Find Camera Hit
#' @description Find the hit using the camera method
#' @param screenR_Object The Object of the package
#' @param model_matrix The matrix that will be used to perform the
#'                     linear model analysis
#' @param contrast A vector or a single value indicating the index or the name
#'                 of the column the model_matrix wo which perform the analysis
#' @importFrom edgeR estimateDisp glmFit glmLRT
#' @param  thresh The threshold
#' @param lfc The Log2FC
#'
#' @return The hit find with the camera method
#' @export
#'
#' @examples

find_camera_hit <- function(screenR_Object, matrix_model, contrast,
                            number_barcode = 3, thresh = 0.0001, lfc = 1 ){
  # We have to convert the screenR obj into an edgeR obj
  DGEList <- create_edgeR_obj(screenR_Object)
  xglm <- edgeR::estimateDisp(DGEList, matrix_model)
  fit <- edgeR::glmFit(xglm, matrix_model)
  lrt <- edgeR::glmLRT(fit, coef=1:length(colnames(matrix_model)))

  camera_hit <- compute_camera(xglm = xglm,
                               lrt = lrt,
                               DGEList = DGEList,
                               matrix_model = matrix_model,
                               contrast = contrast,
                               number_barcode = number_barcode,
                               thresh = thresh,
                               lfc = lfc)
  return(camera_hit)
}

#' @title Compute Camera
#' @description Compute the actual hit using camera
#' @param xglm object created with \code{link{edgeR::estimateDisp}}
#' @param lrt object created with \code{link{edgeR::glmFit}}
#' @param DGEList object of edgeR
#' @param model_matrix the matrix that will be used to perform the
#'                     linear model analysis
#' @param contrast A vector or a single value indicating the index or the name
#'                 of the column the model_matrix wo which perform the analysis
#' @param  thresh The threshold
#' @param lfc The Log2FC
#' @importFrom edgeR estimateDisp glmFit glmLRT
#' @importFrom limma camera
#' @return The hit find with the camera method
#' @export
#'
#' @examples

compute_camera <- function(xglm, lrt, DGEList, matrix_model, contrast,
                           number_barcode = 3, thresh = 0.0001, lfc = 1) {
  # Take all the Tags in discending order
  top <- edgeR::topTags(lrt, n=Inf)
  topids <- top$table[top$table$FDR < thresh & top$table$logFC <= lfc, 1]

  genesymbols <- DGEList$genes[, 1]

  genesymbollist <- unique_gene_symbols(genesymbols, number_barcode)

  camera.res <- limma::camera(xglm, index=genesymbollist,
                       matrix_model, contrast = contrast)
  return(camera.res)
}


# unique_gene_symbols <- function(gene_symbols, number_barcode = 3){
#   gene_symbol_list <- list()
#   un_genesymbols <- unique(gene_symbols)
#   un_genesymbols <- un_genesymbols[!is.na(un_genesymbols)]
#
#   # Take all the gene symbols with a sum greater than 3
#   for(i in un_genesymbols) {
#     sel <- gene_symbols == i & !is.na(gene_symbols)
#     if(sum(sel) > number_barcode)
#       gene_symbol_list[[i]] <- which(sel)
#   }
#   return(gene_symbol_list)
#
# }


unique_gene_symbols <- function(gene_symbols, number_barcode = 3){
  un_genesymbols <- unique(gene_symbols)
  un_genesymbols <- un_genesymbols[!is.na(un_genesymbols)]

  gene_symbol_list  <- lapply(X = un_genesymbols,
                              FUN = select_number_barcode, gene_symbols, number_barcode)
  names(gene_symbol_list) <- un_genesymbols

  gene_symbol_list[sapply(gene_symbol_list, is.null)] <- NULL
  return(gene_symbol_list)
}

select_number_barcode <- function(gene, genesymbols,  number_barcode){
  sel <- genesymbols == gene & !is.na(genesymbols)
  if (sum(sel) > number_barcode) {
    which(sel)
  }
}


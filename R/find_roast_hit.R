#' @title Find Roast Hit
#' @description Find the hit using the Roast method
#' @param screenR_Object The Object of the package
#' @param model_matrix the matrix that will be used to perform the
#'                     linear model analysis
#' @param contrast A vector or a single value indicating the index or the name
#'                 of the column the model_matrix wo which perform the analysis
#' @param nrot Number of rotation to perform the test
#' @importFrom edgeR estimateDisp
#' @importFrom limma mroast
#' @return The hit find with the Roast method
#' @export
#'
#' @examples

find_roast_hit <- function(screenR_Object, matrix_model, contrast,
                           nrot = 9999, number_barcode = 3,
                           direction = "Down", p_val = 0.05){
  DGEList <- create_edgeR_obj(screenR_Object)
  xglm <- edgeR::estimateDisp(DGEList, matrix_model)
  genesymbols <- DGEList$genes[, 1]
  genesymbollist <- unique_gene_symbols(genesymbols, number_barcode)

  roast_hit <- limma::mroast(xglm,
                      index = genesymbollist,
                      design = matrix_model,
                      contrast = contrast,
                      nrot = nrot)
  select <- roast_hit$Direction == "Down" &&
    roast_hit$PValue < 0.05 &&
    roast_hit$NGenes > 3

  roast_hit <-
    roast_hit %>%
    tibble::rownames_to_column("Gene") %>%
    dplyr::tibble() %>%
    dplyr::mutate(Direction = factor(.data$Direction)) %>%
    dplyr::filter(.data$Direction == direction) %>%
    dplyr::filter(.data$PValue < p_val) %>%
    dplyr::filter(.data$NGenes  > number_barcode)


  return(roast_hit)
}









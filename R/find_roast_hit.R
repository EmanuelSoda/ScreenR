#' @title Find Roast Hit
#' @description Find the hit using the Roast method
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenR_object}}
#' @param matrix_model The matrix that will be used to perform the
#'                     linear model analysis. Created using
#'                     \code{\link[stats]{model.matrix}}
#' @param contrast A vector or a single value indicating the index or the name
#'                 of the column the model_matrix to which perform the analysis
#' @param nrot Number of rotation to perform the test
#' @param number_barcode Number of barcode to consider
#' @param direction Direction of variation
#' @param number_barcode The hit find with the Roast method
#' @param p_val P value cut off
#' @importFrom edgeR estimateDisp
#' @importFrom limma mroast
#' @importFrom tibble tibble
#' @return The hits found by ROAST method
#' @concept find
#' @export
#' @examples
#' set.seed(42)
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' matrix_model <- model.matrix(~ slot(object, "groups"))
#' colnames(matrix_model) <- c("Control", "T1_T2", "Treated")
#'
#' result <- find_roast_hit(object,
#'     matrix_model = matrix_model,
#'     contrast = "Treated", nrot = 100
#' )
#' head(result)
find_roast_hit <- function(screenR_Object, matrix_model, contrast,
    nrot = 9999, number_barcode = 3, direction = "Down", p_val = 0.05) {
    DGEList <- create_edgeR_obj(screenR_Object)
    xglm <- edgeR::estimateDisp(DGEList, matrix_model)
    genesymbols <- DGEList$genes[, 1]
    genesymbollist <- unique_gene_symbols(genesymbols, number_barcode)

    roast_hit <- limma::mroast(xglm,
        index = genesymbollist,
        design = matrix_model, contrast = contrast, nrot = nrot
    )

    roast_hit <- roast_hit %>%
        tibble::rownames_to_column("Gene") %>%
        dplyr::tibble() %>%
        dplyr::mutate(Direction = factor(.data$Direction)) %>%
        dplyr::filter(.data$Direction == direction) %>%
        dplyr::filter(.data$PValue < p_val) %>%
        dplyr::filter(.data$NGenes > number_barcode)

    return(roast_hit)
}

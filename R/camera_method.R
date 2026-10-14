#' @title Find Camera Hit
#' @description This function implements the method by proposed by Wu and
#'              Smyth (2012).
#'              The original \code{\link[limma]{camera}} method is a gene set
#'              test, here is applied in the contest of a genetic screening
#'              and so it erforms a competitive barcode set test.
#'              The paper can be found here
#'      \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3458527/}{CAMERA}
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param matrix_model The matrix that will be used to perform the
#'                     linear model analysis created using
#'                     \code{\link[stats]{model.matrix}}
#' @param contrast The comparison to test: the name or index of a column of
#'                 matrix_model, a numeric contrast vector (for example
#'                 created with \code{\link{create_contrast}}) or a
#'                 one-column matrix created with
#'                 \code{\link[limma]{makeContrasts}}.
#' @param thresh Deprecated and ignored: it never had an effect on the
#'               result. Filter the returned table on the FDR column instead.
#' @param lfc Deprecated and ignored: it never had an effect on the result.
#' @param number_barcode Number of barcode that as to be differentially
#'                       expressed (DE)in order to consider the gene associated
#'                       DE. Example a gene is associated
#'                       with 10 shRNA we consider a gene DE if it has at least
#'                       number_barcode = 5 shRNA DE.
#' @param direction String containing the direction of the variation,
#'                  "Down" for the down regulation "Up" for the up regulation.
#' @param fdr The False Discovery Rate threshold: only genes with an FDR
#'            lower or equal to this value are returned. Use fdr = 1 to
#'            return all the genes with the given direction.
#' @param fdr_method How the FDR is computed: "BH" (Benjamini-Hochberg, the
#'                   default) or "storey" (Benjamini-Hochberg multiplied by
#'                   the estimated proportion of genes without effect, less
#'                   conservative when many genes have an effect).
#' @importFrom edgeR estimateDisp
#' @importFrom tibble rownames_to_column
#' @importFrom limma camera
#' @importFrom purrr map_lgl
#' @return A tibble with the hits found using the camera method, with the
#'         columns of \code{\link[limma]{camera}} and Log2FC, the median log2
#'         fold change of the barcodes of the gene. PValue is the one-sided
#'         p-value for the requested direction (camera reports two-sided
#'         p-values) and FDR is computed from it over all the genes.
#'         CAMERA is a competitive test: it compares the barcodes of each
#'         gene with all the other barcodes, so when many genes have an
#'         effect it becomes conservative. \code{\link{find_roast_hit}} with
#'         method = "fry" (self-contained) and
#'         \code{\link{find_gene_zscore_hit}} with non-targeting controls
#'         as reference do not have this limitation.
#' @export
#' @concept find
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#'
#' matrix <- create_design_matrix(object)
#' contrast <- create_contrast(matrix, treatment = "Treated",
#'     control = "Control")
#'
#' result <- find_camera_hit(
#'     screenR_Object = object,
#'     matrix_model = matrix, contrast = contrast
#' )
#' head(result)
find_camera_hit <- function(screenR_Object, matrix_model,
    contrast, number_barcode = 3, thresh = 1e-04, lfc = 1,
    direction = "Down", fdr = 0.05, fdr_method = c("BH", "storey")) {
    fdr_method <- match.arg(fdr_method)
    if (!missing(thresh) || !missing(lfc)) {
        warning("The 'thresh' and 'lfc' arguments of find_camera_hit() are ",
            "deprecated and ignored. Filter the result on the FDR column ",
            "instead.", call. = FALSE)
    }
    contrast <- as_contrast(contrast, matrix_model)
    # We have to convert the screenR obj into an edgeR obj
    DGEList <- ScreenR::create_edger_obj(screenR_Object)
    xglm <- edgeR::estimateDisp(DGEList, matrix_model)

    camera_hit <- compute_camera(
        xglm = xglm, DGEList = DGEList,
        matrix_model = matrix_model, contrast = contrast,
        number_barcode = number_barcode
    )
    effect <- gene_log2fc(
        barcode_log2fc(xglm, matrix_model, contrast), DGEList$genes$Gene
    )
    camera_hit <- camera_hit %>%
        tibble::rownames_to_column("Gene") %>%
        dplyr::tibble() %>%
        dplyr::left_join(effect, by = "Gene") %>%
        dplyr::mutate(Direction = factor(.data$Direction)) %>%
        one_direction(direction, fdr_method) %>%
        dplyr::filter(.data$FDR <= fdr)

    return(camera_hit)
}

#' @title Compute Camera
#' @description This internal function computes the actual hits using  the
#'              camera method.
#' @param xglm object created with \code{\link[edgeR]{estimateDisp}}
#' @param DGEList edgeR object
#' @inheritDotParams find_camera_hit number_barcode contrast matrix_model
#' @return The list of hits found by the camera method
#' @keywords internal
compute_camera <- function(xglm, DGEList, matrix_model, contrast,
    number_barcode = 3) {
    genesymbols <- DGEList$genes$Gene
    genesymbollist <- unique_gene_symbols(genesymbols, number_barcode)

    # Perform the camera test
    camera.res <- run_camera(xglm, genesymbollist, matrix_model, contrast)
    return(camera.res)
}

#' @title Unique gene Symbols
#' @description Compute a unique gene symbol for gene
#' @param gene_symbols The gene symbols list
#' @inheritDotParams  find_camera_hit number_barcode
#' @return A list of unique gene symbols
#' @keywords internal
unique_gene_symbols <- function(gene_symbols, number_barcode = 3) {
    gene_symbols <- as.character(gene_symbols)
    un_genesymbols <- unique(gene_symbols)
    un_genesymbols <- un_genesymbols[!is.na(un_genesymbols)]

    gene_symbol_list <- lapply(
        X = un_genesymbols, FUN = select_number_barcode,
        gene_symbols, number_barcode
    )
    names(gene_symbol_list) <- un_genesymbols
    # Drop the genes without enough barcodes
    gene_symbol_list[purrr::map_lgl(gene_symbol_list, is.null)] <- NULL
    return(gene_symbol_list)
}

#' @title Select  number of Barcode
#' @description Compute a unique gene symbol for gene
#' @param gene The gene name
#' @inheritDotParams unique_gene_symbols gene_symbols number_barcode
#' @return The indices of the barcodes of the gene passed as input, or
#'         NULL if the gene has no more than number_barcode barcodes
#' @keywords internal
select_number_barcode <- function(gene, gene_symbols, number_barcode) {
    sel <- gene_symbols == gene & !is.na(gene_symbols)
    if (sum(sel) > number_barcode) {
        return(which(sel))
    }
    return(NULL)
}

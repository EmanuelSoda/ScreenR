#' @title Find Roast Hit
#' @description Find the hit using the roast method. Roast is a competitive gene
#'              set test which uses rotation instead of permutation. Here is
#'              applied in a contest of a genetic screening so it perform a
#'              barcode competitive test testing for barcode which are
#'              differentially expressed within a gene. More information can be
#'              found in
#'           \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2922896/}{Roast}
#' @param screenR_Object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}
#' @param matrix_model The matrix that will be used to perform the
#'                     linear model analysis. Created using
#'                     \code{\link[stats]{model.matrix}}
#' @param contrast The comparison to test: the name or index of a column of
#'                 matrix_model, a numeric contrast vector (for example
#'                 created with \code{\link{create_contrast}}) or a
#'                 one-column matrix created with
#'                 \code{\link[limma]{makeContrasts}}.
#' @param nrot Number of rotation to perform the test.
#'             Higher number of rotation leads to more statistically significant
#'             result.
#' @param number_barcode Number of barcode that as to be differentially
#'                       expressed (DE)in order to consider the gene associated
#'                       DE. Example a gene is associated
#'                       with 10 shRNA we consider a gene DE if it has at least
#'                       number_barcode = 5 shRNA DE.
#' @param direction Direction of variation
#' @param p_val An optional cut off on the raw (not adjusted) p-value. By
#'              default (NULL) only the FDR cut off is used.
#' @param fdr The False Discovery Rate threshold: only genes with an FDR
#'            lower or equal to this value are returned. Use fdr = 1 to
#'            return all the genes with the given direction.
#' @param seed An optional random seed. ROAST uses random rotations, so the
#'             p-values change slightly between runs; setting a seed makes
#'             the result reproducible. The random number state of the
#'             session is restored afterwards.
#' @importFrom edgeR estimateDisp
#' @importFrom limma mroast
#' @importFrom tibble tibble
#' @return A tibble with the hits found by the ROAST method. Besides the
#'         columns returned by \code{\link[limma]{mroast}} it contains
#'         Log2FC, the median log2 fold change of the barcodes of the gene.
#' @concept find
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' matrix_model <- create_design_matrix(object)
#' contrast <- create_contrast(matrix_model, treatment = "Treated",
#'     control = "Control")
#'
#' result <- find_roast_hit(object,
#'     matrix_model = matrix_model,
#'     contrast = contrast, nrot = 100, seed = 42
#' )
#' head(result)
find_roast_hit <- function(screenR_Object, matrix_model, contrast,
    nrot = 9999, number_barcode = 3, direction = "Down", p_val = NULL,
    fdr = 0.05, seed = NULL) {
    contrast <- as_contrast(contrast, matrix_model)
    DGEList <- create_edger_obj(screenR_Object)
    xglm <- edgeR::estimateDisp(DGEList, matrix_model)
    genesymbols <- DGEList$genes$Gene
    genesymbollist <- unique_gene_symbols(genesymbols, number_barcode)
    if (length(genesymbollist) == 0) {
        stop("No gene has more than number_barcode barcodes: lower ",
            "number_barcode.", call. = FALSE)
    }

    if (!is.null(seed)) {
        restore_random_state()
        set.seed(seed)
    }
    roast_hit <- limma::mroast(xglm,
        index = genesymbollist,
        design = matrix_model, contrast = contrast, nrot = nrot
    )
    effect <- gene_log2fc(
        barcode_log2fc(xglm, matrix_model, contrast), genesymbols
    )

    roast_hit <- roast_hit %>%
        tibble::rownames_to_column("Gene") %>%
        dplyr::tibble() %>%
        dplyr::left_join(effect, by = "Gene") %>%
        dplyr::mutate(Direction = factor(.data$Direction)) %>%
        dplyr::filter(.data$Direction == direction) %>%
        dplyr::filter(.data$FDR <= fdr)
    if (!is.null(p_val)) {
        roast_hit <- dplyr::filter(roast_hit, .data$PValue < p_val)
    }

    return(roast_hit)
}

#' @title Restore the Random State
#' @description Internal function that restores the random number state of
#'              the session when the calling function exits, so that setting
#'              a seed inside a function does not change the random numbers
#'              of the user.
#' @param envir The environment of the calling function.
#' @return NULL, invisibly.
#' @keywords internal
restore_random_state <- function(envir = parent.frame()) {
    global <- globalenv()
    if (exists(".Random.seed", envir = global, inherits = FALSE)) {
        state <- get(".Random.seed", envir = global, inherits = FALSE)
        do.call(on.exit, list(substitute(
            assign(".Random.seed", state, envir = globalenv())
        ), add = TRUE), envir = envir)
    } else {
        do.call(on.exit, list(quote(
            if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
                rm(".Random.seed", envir = globalenv())
            }
        ), add = TRUE), envir = envir)
    }
    invisible(NULL)
}

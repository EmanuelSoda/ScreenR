#' @title Find hits with CAMERA
#' @description Finds the genes whose barcodes are depleted (or enriched) in
#'     the comparison given by the contrast, using the
#'     \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3458527/}{CAMERA}
#'     method of Wu and Smyth (2012), \code{\link[limma]{camera}}. CAMERA is a
#'     gene set test: here each gene is a set of barcodes, and the barcodes of
#'     the gene are compared with all the other barcodes (a competitive test).
#'     Unlike \code{\link{find_roast_hit}}, it also works with a single
#'     control sample (for example a plasmid library).
#' @details The raw counts are modelled with edgeR (the dispersion is
#'     estimated with \code{\link[edgeR]{estimateDisp}}), and camera tests
#'     the barcodes of each gene together, accounting for the correlation
#'     between them. The two-sided p-values of camera are converted into
#'     one-sided p-values for the requested direction and the FDR is computed
#'     from them over all the tested genes.
#'
#'     CAMERA is a competitive test: it compares the barcodes of each gene with
#'     all the other barcodes, so when many genes have an effect it becomes
#'     conservative. \code{\link{find_roast_hit}} with method = "fry"
#'     (self-contained) does not have this limitation, and neither does
#'     \code{\link{find_gene_zscore_hit}} with control genes as reference, as
#'     long as the control genes behave like genes without effect (see its
#'     documentation).
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param matrix_model The design matrix of the linear model, for example
#'     created with \code{\link{create_design_matrix}} (or with
#'     \code{\link[stats]{model.matrix}}).
#' @param contrast The comparison to test: the name or index of a column of
#'     matrix_model, a numeric contrast vector (for example created with
#'     \code{\link{create_contrast}}) or a one-column matrix created with
#'     \code{\link[limma]{makeContrasts}}.
#' @param thresh Deprecated and ignored: it never had an effect on the
#'     result. Filter the returned table on the FDR column instead.
#' @param lfc Deprecated and ignored: it never had an effect on the result.
#' @param number_barcode Only genes with more than number_barcode barcodes
#'     are tested (default 3, so a gene needs at least 4 barcodes). The
#'     barcodes of a gene are tested together as a set, so this is not a
#'     number of barcodes that must change.
#' @param direction The direction of the change: "Down" (default) for
#'     depleted genes or "Up" for enriched genes.
#' @param fdr The FDR threshold: only genes with an FDR lower than or equal
#'     to this value are returned (default 0.05). Use fdr = 1 to return all
#'     the genes with the given direction.
#' @param fdr_method How the FDR is computed: "BH" (Benjamini-Hochberg, the
#'     default) or "storey" (Benjamini-Hochberg multiplied by the estimated
#'     proportion of genes without effect, less conservative when many genes
#'     have an effect).
#' @importFrom edgeR estimateDisp
#' @importFrom tibble rownames_to_column
#' @importFrom limma camera
#' @importFrom purrr map_lgl
#' @return A tibble with one row for each hit in the requested direction and
#'     the columns Gene, NGenes (the number of barcodes of the gene),
#'     Direction, PValue (the one-sided p-value for the requested direction),
#'     FDR and Log2FC (the median log2 fold change of the barcodes of the
#'     gene).
#' @seealso \code{\link{find_roast_hit}}, \code{\link{find_gene_zscore_hit}},
#'     \code{\link{find_time_hit}}, \code{\link{summarise_hits}},
#'     \code{\link{plot_barcode_hit}}, \code{\link{create_design_matrix}},
#'     \code{\link{create_contrast}}
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
    direction = c("Down", "Up"), fdr = 0.05,
    fdr_method = c("BH", "storey")) {
    direction <- match.arg(direction)
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

#' @title Barcodes of each gene
#' @description Internal function that returns, for each gene with more than
#'     number_barcode barcodes, the indices of its barcodes.
#' @param gene_symbols The gene of each barcode.
#' @inheritDotParams  find_camera_hit number_barcode
#' @return A named list with the indices of the barcodes of each gene.
#' @keywords internal
unique_gene_symbols <- function(gene_symbols, number_barcode = 3) {
    gene_symbols <- as.character(gene_symbols)
    un_genesymbols <- unique(gene_symbols)
    un_genesymbols <- un_genesymbols[!is.na(un_genesymbols)]
    check_barcode_threshold(table(gene_symbols[!is.na(gene_symbols)]),
        number_barcode, strict = TRUE, argument = "number_barcode")

    gene_symbol_list <- lapply(
        X = un_genesymbols, FUN = select_number_barcode,
        gene_symbols, number_barcode
    )
    names(gene_symbol_list) <- un_genesymbols
    # Drop the genes without enough barcodes
    gene_symbol_list[purrr::map_lgl(gene_symbol_list, is.null)] <- NULL
    return(gene_symbol_list)
}

#' @title Select the barcodes of a gene
#' @description Internal function that returns the indices of the barcodes of
#'     a gene, when the gene has more than number_barcode barcodes.
#' @param gene The gene name
#' @inheritDotParams unique_gene_symbols gene_symbols number_barcode
#' @return The indices of the barcodes of the gene passed as input, or
#'     NULL if the gene has no more than number_barcode barcodes
#' @keywords internal
select_number_barcode <- function(gene, gene_symbols, number_barcode) {
    sel <- gene_symbols == gene & !is.na(gene_symbols)
    if (sum(sel) > number_barcode) {
        return(which(sel))
    }
    return(NULL)
}

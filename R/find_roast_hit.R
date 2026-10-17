#' @title Find hits with ROAST or FRY
#' @description Finds the genes whose barcodes are depleted (or enriched) in
#'     the comparison given by the contrast, using the
#'     \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2922896/}{ROAST}
#'     method (\code{\link[limma]{mroast}}) or its fast approximation FRY
#'     (\code{\link[limma]{fry}}). ROAST is a self-contained gene set test:
#'     it tests whether the barcodes of a gene change, without comparing them
#'     with the other barcodes, so it is not affected by the number of genes
#'     with an effect. It needs replicates in both groups (see Details).
#' @details The raw counts are modelled with edgeR (the dispersion is
#'     estimated with \code{\link[edgeR]{estimateDisp}}) and the barcodes of
#'     each gene are tested together. The p-values are computed with random
#'     rotations (method = "roast", whose smallest possible p-value is
#'     1 / (nrot + 1)) or with \code{\link[limma]{fry}} (method = "fry"), a
#'     fast and exact approximation of roast with an unlimited number of
#'     rotations, which is faster and gives continuous p-values. The two-sided
#'     p-values are converted into one-sided p-values for the requested
#'     direction and the FDR is computed from them over all the tested genes.
#'
#'     Since roast tests each gene on its own barcodes, its power depends on
#'     the variability between replicates: it needs replicates in both the
#'     treated and the control group and has little power with two replicates
#'     or noisy data. With a single control sample (for example a plasmid
#'     library) it finds almost no hits; in this case use
#'     \code{\link{find_camera_hit}} or \code{\link{find_gene_zscore_hit}},
#'     which compare each gene with the other genes. Being self-contained,
#'     roast also reports genes whose barcodes change because of off-target
#'     effects.
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}, normalized with
#'     \code{\link{normalize_data}}. The model uses its raw counts with its
#'     effective library sizes as offsets, the same normalization as the plots
#'     and the data table (see \code{\link{create_edger_obj}}).
#' @param matrix_model The design matrix of the linear model, for example
#'     created with \code{\link{create_design_matrix}} (or with
#'     \code{\link[stats]{model.matrix}}).
#' @param contrast The comparison to test: the name or index of a column of
#'     matrix_model, a numeric contrast vector (for example created with
#'     \code{\link{create_contrast}}) or a one-column matrix created with
#'     \code{\link[limma]{makeContrasts}}.
#' @param nrot The number of random rotations used by method = "roast"
#'     (default 9999). More rotations give more precise p-values and allow
#'     smaller ones (the smallest is 1 / (nrot + 1)), but take longer. Not
#'     used by method = "fry".
#' @param number_barcode Only genes with more than number_barcode barcodes
#'     are tested (default 3, so a gene needs at least 4 barcodes). The
#'     barcodes of a gene are tested together as a set, so this is not a
#'     number of barcodes that must change.
#' @param direction The direction of the change: "Down" (default) for
#'     depleted genes or "Up" for enriched genes.
#' @param p_val An optional cut off on the raw (not adjusted) one-sided
#'     p-value: only genes with PValue < p_val are returned. By default (NULL)
#'     only the FDR cut off is used.
#' @param fdr The FDR threshold: only genes with an FDR lower than or equal
#'     to this value are returned (default 0.05). Use fdr = 1 to return all
#'     the genes with the given direction.
#' @param seed An optional random seed. ROAST uses random rotations, so the
#'     p-values change slightly between runs; setting a seed makes the result
#'     reproducible. The random number state of the session is restored
#'     afterwards. Not used by method = "fry".
#' @param method "roast" (default, random rotations) or "fry" (fast exact
#'     approximation, recommended for large screens).
#' @param fdr_method How the FDR is computed: "BH" (Benjamini-Hochberg, the
#'     default) or "storey" (Benjamini-Hochberg multiplied by the estimated
#'     proportion of genes without effect, less conservative when many genes
#'     have an effect).
#' @importFrom edgeR estimateDisp
#' @importFrom limma mroast fry
#' @importFrom tibble tibble
#' @return A tibble with one row for each hit in the requested direction and
#'     the columns returned by \code{\link[limma]{mroast}} (Gene, NGenes,
#'     PropDown, PropUp, Direction, PValue, FDR, PValue.Mixed, FDR.Mixed) or
#'     by \code{\link[limma]{fry}} (without PropDown and PropUp), plus Log2FC,
#'     the median log2 fold change of the barcodes of the gene. NGenes is the
#'     number of barcodes of the gene. PValue is the one-sided p-value for
#'     the requested direction and FDR is computed from it over all the
#'     genes; PValue.Mixed and FDR.Mixed are those of limma (a change in
#'     either direction).
#' @seealso \code{\link{find_camera_hit}}, \code{\link{find_gene_zscore_hit}},
#'     \code{\link{find_time_hit}}, \code{\link{summarise_hits}},
#'     \code{\link{plot_barcode_hit}}, \code{\link{create_design_matrix}},
#'     \code{\link{create_contrast}}
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
#'
#' # FRY: faster, with continuous p-values (fdr = 1 returns all the genes)
#' result <- find_roast_hit(object,
#'     matrix_model = matrix_model,
#'     contrast = contrast, method = "fry", fdr = 1
#' )
#' head(result)
find_roast_hit <- function(screenR_Object, matrix_model, contrast,
    nrot = 9999, number_barcode = 3, direction = c("Down", "Up"),
    p_val = NULL, fdr = 0.05, seed = NULL, method = c("roast", "fry"),
    fdr_method = c("BH", "storey")) {
    direction <- match.arg(direction)
    method <- match.arg(method)
    fdr_method <- match.arg(fdr_method)
    contrast <- as_contrast(contrast, matrix_model)
    DGEList <- create_edger_obj(screenR_Object)
    xglm <- edgeR::estimateDisp(DGEList, matrix_model)
    genesymbols <- DGEList$genes$Gene
    genesymbollist <- unique_gene_symbols(genesymbols, number_barcode)
    if (length(genesymbollist) == 0) {
        stop("No gene has more than number_barcode barcodes: lower ",
            "number_barcode.", call. = FALSE)
    }

    if (method == "roast") {
        if (!is.null(seed)) {
            restore_random_state()
            set.seed(seed)
        }
        roast_hit <- limma::mroast(xglm,
            index = genesymbollist,
            design = matrix_model, contrast = contrast, nrot = nrot
        )
    } else {
        roast_hit <- limma::fry(xglm,
            index = genesymbollist,
            design = matrix_model, contrast = contrast
        )
    }
    effect <- gene_log2fc(
        barcode_log2fc(xglm, matrix_model, contrast), genesymbols
    )

    roast_hit <- roast_hit %>%
        tibble::rownames_to_column("Gene") %>%
        dplyr::tibble() %>%
        dplyr::left_join(effect, by = "Gene") %>%
        dplyr::mutate(Direction = factor(.data$Direction)) %>%
        one_direction(direction, fdr_method) %>%
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

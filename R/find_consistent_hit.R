#' Combine evidence for an effect in every contrast
#'
#' Tests whether each gene has an effect in a prespecified direction in all
#' supplied contrasts, using an intersection-union test.
#' @param results A named list of at least two complete gene-level tables.
#'     Every table must contain the same unique, non-missing Gene identifiers
#'     and numeric PValue values between zero and one. PValue must be an
#'     unadjusted one-sided p-value for direction, including genes whose
#'     observed effect is opposite. Do not supply filtered hit lists or
#'     two-sided p-values. Optional Log2FC columns are retained.
#' @param direction The common, prespecified direction, "Down" or "Up".
#'     This labels the hypothesis; it does not convert input p-values. The
#'     caller must ensure that all tables test this direction.
#' @param fdr Maximum combined FDR to return (default 0.05); use 1 for all
#'     tested genes.
#' @param fdr_method Multiple-testing correction across genes: "BH" (default,
#'     independence or suitable positive dependence) or "BY" (arbitrary
#'     dependence).
#' @details For each gene the null is that at least one contrast has no
#'     effect in the requested direction. The alternative is an effect in
#'     that direction in every contrast. The combined p-value is the maximum
#'     of the individual raw p-values. This combination allows arbitrary
#'     dependence between contrasts, provided the individual p-values are
#'     valid. It does not repair a misspecified model, an inappropriate
#'     reference distribution, or ignored repeated measurements.
#'
#'     FDR adjustment is applied to the combined p-values over all supplied
#'     genes, before filtering. Input FDR columns are ignored. Missing genes,
#'     missing p-values and duplicated identifiers are errors: a missing test
#'     cannot be silently omitted from an all-contrasts hypothesis. The
#'     function cannot detect tables filtered to the same subset of genes;
#'     supplying the complete testing universe is the caller's responsibility.
#'
#'     Consistency here means an effect in every specified contrast, not
#'     equal effect sizes, a time interaction, or independent replication.
#' @return A tibble ordered by combined PValue, with Gene, Direction (the
#'     tested direction), NContrasts, PValue, FDR, PValue_<contrast> and,
#'     when supplied, Log2FC_<contrast> columns. No pooled effect is computed.
#' @references Heller R, Golland Y, Malach R, Benjamini Y (2007).
#'     Conjunction group analysis: an alternative to mixed/random effect
#'     analysis. NeuroImage 37:1178-1185.
#'     \url{https://pubmed.ncbi.nlm.nih.gov/17689266/}
#' @seealso \code{\link{find_consistent_hit}}, \code{\link{find_time_hit}}
#' @concept find
#' @export
#' @examples
#' t3 <- data.frame(Gene = c("A", "B"), PValue = c(0.001, 0.01))
#' t4 <- data.frame(Gene = c("B", "A"), PValue = c(0.8, 0.002))
#' combine_consistent_hits(list(T3 = t3, T4 = t4))
combine_consistent_hits <- function(results, direction = c("Down", "Up"),
    fdr = 0.05, fdr_method = c("BH", "BY")) {
    direction <- match.arg(direction)
    fdr_method <- match.arg(fdr_method)
    check_consistent_names(results, "results")
    if (!is.numeric(fdr) || length(fdr) != 1L || !is.finite(fdr) ||
        fdr < 0 || fdr > 1) {
        stop("fdr must be a number between 0 and 1.", call. = FALSE)
    }
    genes <- NULL
    for (name in names(results)) {
        table <- results[[name]]
        if (!is.data.frame(table) ||
            !all(c("Gene", "PValue") %in% names(table))) {
            stop("Each result must be a table with Gene and PValue: ", name,
                call. = FALSE)
        }
        ids <- as.character(table$Gene)
        if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
            stop("Gene identifiers must be unique and non-missing: ", name,
                call. = FALSE)
        }
        p <- table$PValue
        if (!is.numeric(p) || any(!is.finite(p)) || any(p < 0 | p > 1)) {
            stop("PValue must contain finite numbers between 0 and 1: ",
                name, call. = FALSE)
        }
        if (is.null(genes)) genes <- ids
        if (!setequal(genes, ids)) {
            stop("All results must contain the same genes; supply complete ",
                "unfiltered tables. Different genes in: ", name,
                call. = FALSE)
        }
    }
    result <- tibble::tibble(Gene = genes,
        Direction = rep(direction, length(genes)),
        NContrasts = rep(length(results), length(genes)))
    pvalues <- lapply(results, function(table) {
        table$PValue[match(genes, as.character(table$Gene))]
    })
    result$PValue <- Reduce(pmax, pvalues)
    result$FDR <- stats::p.adjust(result$PValue, method = fdr_method)
    for (name in names(results)) {
        table <- results[[name]]
        result[[paste0("PValue_", name)]] <- pvalues[[name]]
        if ("Log2FC" %in% names(table)) {
            result[[paste0("Log2FC_", name)]] <-
                table$Log2FC[match(genes, as.character(table$Gene))]
        }
    }
    result <- result[order(result$PValue, result$Gene), ]
    result[result$FDR <= fdr, ]
}

#' Find genes with a consistent directional effect across contrasts
#'
#' Runs the same gene-level method for every contrast of one design matrix,
#' then tests for an effect in all contrasts with
#' \code{\link{combine_consistent_hits}}.
#' @param screenR_Object A normalized ScreenR object.
#' @param matrix_model A design matrix with rows in the sample order of the
#'     object, with sample identifiers as row names. Mismatched names or
#'     order are errors (see \code{\link{create_design_matrix}}).
#'     Use a design appropriate for the experimental dependence.
#' @param contrasts A uniquely named list of at least two contrasts accepted
#'     by \code{\link{find_roast_hit}}, such as treatment versus control at
#'     each time point. The same design and normalization are used throughout.
#' @param method "fry" (default), "roast", "camera", or "zscore".
#'     The zscore method first uses \code{\link{compute_barcode_statistics}}
#'     to obtain replicate-aware barcode Z-scores, then runs
#'     \code{\link{find_gene_zscore_hit}}. Both reference = "random" (its
#'     default) and reference = "empirical" are supported through method_args.
#' @param direction The prespecified direction, "Down" or "Up".
#' @param fdr Maximum combined FDR to return; 1 returns all tested genes.
#' @param fdr_method "BH" or "BY" for the combined test across genes.
#' @param method_args A named list of additional arguments for the selected
#'     method, for example list(number_barcode = 3),
#'     list(nrot = 9999, seed = 42) for ROAST, or
#'     list(reference = "empirical", min_barcodes = 4) for Z-score.
#'     Arguments controlling the object, model, contrast, direction or
#'     filtering cannot be overridden. Z-score uses score = "Zscore".
#' @details FRY and ROAST test a self-contained effect in every contrast;
#'     CAMERA tests enrichment relative to the other genes in every contrast.
#'     The Z-score interpretation depends on its chosen reference and its
#'     calibration. These are different hypotheses. Legacy barcode-count
#'     rules have no gene-level p-values and are not supported.
#'
#'     All tested genes, including opposite observed directions, are retained
#'     for combination. The existing single-contrast methods keep their usual
#'     defaults. Different gene universes across contrasts are an error.
#'     For precomputed complete p-value tables, use combine_consistent_hits().
#' @return The tibble described in \code{\link{combine_consistent_hits}}.
#' @seealso \code{\link{find_roast_hit}}, \code{\link{find_camera_hit}},
#'     \code{\link{find_gene_zscore_hit}}, \code{\link{find_time_hit}}
#' @concept find
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' design <- create_design_matrix(object)
#' contrasts <- list(
#'     Treatment = create_contrast(design, "Treated", "Control"),
#'     Baseline = create_contrast(design, "Treated", "T1/T2")
#' )
#' find_consistent_hit(object, design, contrasts, method = "fry")
find_consistent_hit <- function(screenR_Object, matrix_model, contrasts,
    method = c("fry", "roast", "camera", "zscore"),
    direction = c("Down", "Up"), fdr = 0.05,
    fdr_method = c("BH", "BY"), method_args = list()) {
    method <- match.arg(method)
    direction <- match.arg(direction)
    fdr_method <- match.arg(fdr_method)
    check_consistent_names(contrasts, "contrasts")
    if (!is.numeric(fdr) || length(fdr) != 1L || !is.finite(fdr) ||
        fdr < 0 || fdr > 1) {
        stop("fdr must be a number between 0 and 1.", call. = FALSE)
    }
    fun <- switch(method, fry = find_roast_hit, roast = find_roast_hit,
        camera = find_camera_hit, zscore = find_gene_zscore_hit)
    reserved <- c("screenR_Object", "matrix_model", "contrast", "direction",
        "fdr", "fdr_method", "keep_all", "p_val", "method",
        "table_treate_vs_control", "score", "thresh", "lfc")
    if (!is.list(method_args) || (length(method_args) > 0 &&
        (is.null(names(method_args)) || anyNA(names(method_args)) ||
        any(names(method_args) == "") || anyDuplicated(names(method_args))))) {
        stop("method_args must be a uniquely named list.", call. = FALSE)
    }
    bad <- setdiff(names(method_args), setdiff(names(formals(fun)), reserved))
    if (length(bad)) {
        stop("Unsupported or reserved method_args: ", paste(bad, collapse = ", "),
            call. = FALSE)
    }
    matrix_model <- check_design_samples(matrix_model, screenR_Object)
    contrasts <- lapply(contrasts, as_contrast, design = matrix_model)
    results <- lapply(contrasts, function(contrast) {
        args <- list(direction = direction, fdr = 1, fdr_method = "BH")
        if (method == "zscore") {
            args$table_treate_vs_control <- compute_barcode_statistics(
                screenR_Object, matrix_model, contrast)
        } else {
            args$screenR_Object <- screenR_Object
            args$matrix_model <- matrix_model
            args$contrast <- contrast
            args$keep_all <- TRUE
            if (method %in% c("fry", "roast")) args$method <- method
        }
        do.call(fun, c(args, method_args))
    })
    combine_consistent_hits(results, direction, fdr, fdr_method)
}

check_consistent_names <- function(x, what) {
    if (!is.list(x) || is.data.frame(x) || length(x) < 2L ||
        is.null(names(x)) || anyNA(names(x)) || any(names(x) == "") ||
        anyDuplicated(names(x))) {
        stop(what, " must be a uniquely named list of at least two entries.",
            call. = FALSE)
    }
    invisible(NULL)
}

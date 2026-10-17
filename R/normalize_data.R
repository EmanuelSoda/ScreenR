#' @title Normalize the counts
#' @description Sets the normalization of a ScreenR object, so that samples
#'     sequenced at different depths can be compared. The raw counts are not
#'     changed: the normalization is the effective library size of each
#'     sample, and the normalized count of a barcode is its raw count divided
#'     by the effective library size of its sample, times 1e6 (counts per
#'     million). Three methods are available:
#'     \itemize{
#'       \item "cpm" (default): the effective library size is the total
#'             number of reads of the sample;
#'       \item "tmm": the total number of reads multiplied by the trimmed
#'             mean of M-values (TMM) normalization factor of edgeR
#'             (\code{\link[edgeR]{normLibSizes}}). This is more robust when
#'             strong hits deplete a large part of the library;
#'       \item "control": the number of reads of the control barcodes (for
#'             example non-targeting shRNAs, or the sgRNAs of non-essential
#'             genes) of the sample. This assumes that the control barcodes
#'             are not affected by the treatment.
#'     }
#' @details The normalization is used consistently by the whole analysis:
#'     the normalized counts (\code{\link{get_normalized_count_table}}), the
#'     data table (\code{\link{get_data_table}}) and everything computed from
#'     it (\code{\link{compute_metrics}}, \code{\link{plot_trend}},
#'     \code{\link{plot_boxplot}}, \code{\link{compute_slope}}, ...), the
#'     sample plots (\code{\link{plot_mds}}, \code{\link{plot_pca}},
#'     \code{\link{plot_replicate_correlation}}) and the edgeR and limma
#'     models (\code{\link{create_edger_obj}}, used by
#'     \code{\link{find_camera_hit}}, \code{\link{find_roast_hit}},
#'     \code{\link{compute_barcode_statistics}}, \code{\link{find_time_hit}},
#'     \code{\link{filter_by_variance}} and \code{\link{plot_barcode_hit}}),
#'     whose offsets are the logarithms of the effective library sizes. The
#'     models always use the raw counts. Logarithms and pseudocounts used by
#'     the plots (for example log2(normalized + 1) in \code{\link{plot_pca}})
#'     are display transformations: they do not change the normalization.
#'
#'     It is stored in the object: the effective library sizes in the
#'     EffectiveLibSize column of the colData, the method and its provenance
#'     in the metadata (see \code{\link{get_normalization}}). Replacing the
#'     counts removes it; subsetting the object keeps the effective library
#'     sizes of the retained samples (call normalize_data() again to
#'     normalize on the retained barcodes only). Calling normalize_data()
#'     again replaces the normalization, and every view of the object follows
#'     immediately.
#'
#'     Every sample must have reads (and, for "control", reads of the control
#'     barcodes): a sample with an effective library size of zero cannot be
#'     normalized and is an error.
#'
#'     \strong{Choosing the method.} In a dropout screen many barcodes are
#'     depleted, so with "cpm" all the other barcodes look enriched. "tmm"
#'     reduces this bias, but it assumes that most barcodes do not change and
#'     that the changes go in both directions: when many barcodes change in
#'     the same direction it corrects only part of the bias (in a simulation
#'     with 20\% of the genes depleted 8 times it removed about half of it).
#'     "control" removes it when control barcodes known to be without effect
#'     are available (for example the sgRNAs of non-essential genes).
#' @param screenR_Object The ScreenR object obtained using
#'     \code{\link{create_screenr_object}}.
#' @param method The normalization method: "cpm" (default), "tmm" or
#'     "control".
#' @param control_barcodes The identifiers of the control barcodes for the
#'     "control" method (required by that method, ignored by the others).
#'     Identifiers that are not in the object give a warning; if none is in
#'     the object, it is an error.
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble
#' @importFrom purrr map_if
#' @importFrom edgeR DGEList normLibSizes
#' @return The normalized ScreenR object.
#' @seealso \code{\link{get_normalization}}, \code{\link{create_edger_obj}},
#'     \code{\link{get_normalized_count_table}},
#'     \code{\link{remove_all_zero_row}}
#' @export
#' @concept  compute
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' object <- normalize_data(object)
#' get_normalization(object)$method
#' get_normalized_count_table(object)
#'
#' # TMM normalization: the same effective library sizes are used by the
#' # plots and by the edgeR models
#' object <- normalize_data(object, method = "tmm")
#' get_normalization(object)$effective_library_size
#' create_edger_obj(object)$samples
normalize_data <- function(screenR_Object, method = c("cpm", "tmm", "control"),
    control_barcodes = NULL) {
    method <- match.arg(method)
    object <- update_screenr_object(screenR_Object)
    if (nrow(object) == 0 || ncol(object) == 0) {
        stop("The object has no barcodes or no samples to normalize.",
            call. = FALSE)
    }
    counts <- SummarizedExperiment::assay(object, "counts",
        withDimnames = TRUE)
    library_size <- colSums(counts)
    empty <- colnames(counts)[library_size <= 0]
    if (length(empty) > 0) {
        stop("Sample(s) ", paste(empty, collapse = ", "), " have no reads: ",
            "they cannot be normalized. Remove them from the object.",
            call. = FALSE)
    }
    factors <- rep(1, ncol(counts))
    reference <- NULL

    if (method == "cpm") {
        sizes <- library_size
    } else if (method == "tmm") {
        dge <- edgeR::normLibSizes(edgeR::DGEList(counts = counts))
        factors <- dge$samples$norm.factors
        sizes <- library_size * factors
    } else {
        reference <- control_reference(object, control_barcodes)
        sizes <- colSums(counts[reference, , drop = FALSE])
        bad <- colnames(counts)[!is.finite(sizes) | sizes <= 0]
        if (length(bad) > 0) {
            stop("The control barcodes have no reads in sample(s) ",
                paste(bad, collapse = ", "), ": they cannot be used to ",
                "normalize.", call. = FALSE)
        }
    }

    state <- list(
        method = method,
        reference = reference,
        library_size = library_size,
        norm_factors = stats::setNames(factors, colnames(counts)),
        n_barcodes = nrow(counts),
        screenr_version = screenr_version()
    )
    object <- remove_normalization(object)
    set_normalization_state(object, sizes, state)
}

#' @title Control Barcodes
#' @description Internal function that checks the control barcodes of the
#'              "control" normalization against the barcodes of the object.
#' @param object A ScreenR object.
#' @param control_barcodes The identifiers of the control barcodes.
#' @return The identifiers of the control barcodes that are in the object.
#' @keywords internal
control_reference <- function(object, control_barcodes) {
    if (is.null(control_barcodes) || length(control_barcodes) == 0) {
        stop("control_barcodes must be given when method = 'control'.",
            call. = FALSE)
    }
    identifiers <- unique(as_barcode_id(control_barcodes,
        "control barcodes", allow_missing = TRUE))
    identifiers <- identifiers[!is.na(identifiers) & identifiers != ""]
    present <- identifiers[identifiers %in% rownames(object)]
    if (length(present) == 0) {
        stop("None of the control_barcodes is in the count table.",
            call. = FALSE)
    }
    missing <- setdiff(identifiers, present)
    if (length(missing) > 0) {
        warning(length(missing), " of the ", length(identifiers),
            " control barcodes are not in the object (",
            paste(utils::head(missing, 5), collapse = ", "),
            if (length(missing) > 5) ", ..." else "",
            "): the other ", length(present), " are used.", call. = FALSE)
    }
    present
}

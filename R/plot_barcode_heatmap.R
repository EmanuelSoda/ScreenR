#' Plot barcode abundance across samples
#'
#' Displays the barcodes of selected genes as a heatmap of log2 normalized
#' abundance. Gene panels retain individual reagents and sample replicates.
#' @param screenR_Object A normalized ScreenR object.
#' @param genes The gene identifiers to show, in panel order. All must be
#'     present in the object.
#' @param samples Optional sample identifiers in the desired column order.
#'     NULL uses all samples in object order. Within column_group panels,
#'     the supplied order is preserved.
#' @param center "barcode" (default): subtract each barcode's mean log2
#'     abundance across the displayed samples. "reference": subtract its
#'     mean log2 abundance across reference_samples. "none": display log2
#'     normalized abundance without centering.
#' @param reference_samples Sample identifiers for center = "reference".
#'     They must be in the object but need not be displayed. Their log2
#'     abundances are averaged, equivalent to a geometric-mean reference
#'     after adding the pseudocount. All-zero reference barcodes are allowed
#'     and their display depends strongly on the pseudocount.
#' @param pseudocount Positive finite value added to normalized counts before
#'     log2 transformation (default 1). This affects the display only.
#' @param column_group A colData variable for column panels, "Day" by
#'     default. NULL draws a single column panel. Missing values are errors.
#' @param barcode_labels Whether to label the barcode rows (default TRUE).
#' @details The default centering emphasizes within-barcode patterns, not
#'     absolute abundance or significance. Its zero is the mean of the
#'     displayed samples, so changing the selection changes the reference.
#'     Neither the colors nor the number of barcode rows represent independent
#'     biological replication. No clustering or hypothesis test is performed.
#'     The centered color scale is symmetric about zero; the uncentered
#'     scale is sequential. The raw counts and normalization are unchanged.
#'     No theme is applied; choose appearance in user code with theme_set().
#' @return A ggplot object. Its data contain Gene, Barcode, Sample,
#'     ColumnGroup, Log2Normalized and Value (the displayed quantity).
#' @seealso \code{\link{plot_trend}}, \code{\link{plot_barcode_hit}},
#'     \code{\link{plot_boxplot}}
#' @concept plot
#' @export
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' plot_barcode_heatmap(object, genes = c("Gene_42", "Gene_100"))
plot_barcode_heatmap <- function(screenR_Object, genes, samples = NULL,
    center = c("barcode", "reference", "none"), reference_samples = NULL,
    pseudocount = 1, column_group = "Day", barcode_labels = TRUE) {
    center <- match.arg(center)
    object <- update_screenr_object(screenR_Object)
    if (!is.numeric(pseudocount) || length(pseudocount) != 1L ||
        !is.finite(pseudocount) || pseudocount <= 0) {
        stop("pseudocount must be a positive finite number.", call. = FALSE)
    }
    if (!is.logical(barcode_labels) || length(barcode_labels) != 1L ||
        is.na(barcode_labels)) {
        stop("barcode_labels must be TRUE or FALSE.", call. = FALSE)
    }
    valid_ids <- function(ids, available, what) {
        if (!is.character(ids) || length(ids) == 0L || anyNA(ids) ||
            any(!nzchar(ids)) || anyDuplicated(ids) ||
            any(!ids %in% available)) {
            stop(what, " must be unique, non-missing identifiers present ",
                "in the object.", call. = FALSE)
        }
    }
    gene_ids <- genes_of(object)
    valid_ids(genes, gene_ids, "genes")
    if (is.null(samples)) samples <- colnames(object)
    valid_ids(samples, colnames(object), "samples")
    if (center == "reference") {
        valid_ids(reference_samples, colnames(object), "reference_samples")
    } else if (!is.null(reference_samples)) {
        stop("reference_samples requires center = 'reference'.", call. = FALSE)
    }
    selected <- which(gene_ids %in% genes)
    values <- log2(normalized_counts(object,
        "plot_barcode_heatmap()")[selected, , drop = FALSE] + pseudocount)
    shown <- values[, samples, drop = FALSE]
    baseline <- switch(center,
        barcode = rowMeans(shown),
        reference = rowMeans(values[, reference_samples, drop = FALSE]),
        none = rep(0, nrow(shown)))
    centered <- sweep(shown, 1, baseline, "-")
    info <- SummarizedExperiment::colData(object)
    if (is.null(column_group)) {
        groups <- rep("Samples", length(samples))
    } else {
        if (!is.character(column_group) || length(column_group) != 1L ||
            is.na(column_group) || !column_group %in% colnames(info)) {
            stop("column_group must name a colData variable, or be NULL.",
                call. = FALSE)
        }
        groups <- as.character(info[[column_group]][match(samples,
            colnames(object))])
        if (anyNA(groups)) {
            stop("column_group has missing values in the selected samples.",
                call. = FALSE)
        }
    }
    data <- tibble::tibble(
        Gene = factor(rep(gene_ids[selected], each = length(samples)),
            levels = genes),
        Barcode = factor(rep(rownames(shown), each = length(samples)),
            levels = rev(rownames(shown))),
        Sample = factor(rep(samples, times = nrow(shown)), levels = samples),
        ColumnGroup = factor(rep(groups, times = nrow(shown)),
            levels = unique(groups)),
        Log2Normalized = as.vector(t(shown)),
        Value = as.vector(t(centered)))
    plot <- ggplot2::ggplot(data, aes(.data$Sample, .data$Barcode,
        fill = .data$Value)) +
        ggplot2::geom_tile() +
        ggplot2::facet_grid(Gene ~ ColumnGroup, scales = "free", space = "free") +
        ggplot2::labs(x = NULL, y = "Barcode")
    if (center == "none") {
        plot <- plot + ggplot2::scale_fill_viridis_c(
            name = "Log2 normalized\nabundance")
    } else {
        limit <- max(abs(data$Value))
        if (limit == 0) limit <- 1
        label <- if (center == "barcode") {
            "Log2 abundance\nminus barcode mean"
        } else "Log2 abundance\nminus reference mean"
        plot <- plot + ggplot2::scale_fill_gradient2(low = "#2166AC",
            mid = "white", high = "#B2182B", midpoint = 0,
            limits = c(-limit, limit), name = label)
    }
    if (!barcode_labels) {
        plot <- plot + ggplot2::scale_y_discrete(breaks = NULL)
    }
    plot
}
